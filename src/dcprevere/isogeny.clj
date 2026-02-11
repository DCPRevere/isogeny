#!/usr/bin/env bb
;; -*- mode: clojure -*-

(ns dcprevere.isogeny
  (:gen-class)
  (:require
   [babashka.fs :as fs]
   [clojure.java.shell :as shell]
   [cheshire.core :as json]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]
   [clojure.tools.logging :as log]
   [selmer.parser :as s.parser]
   [selmer.util :as s.util]))

(def verbose? (atom false))
(def safe? (atom false))
(def dry-run? (atom false))

(def usage
  "USAGE: isogeny render -t TEMPLATE -c CONTEXT -o OUTPUT
       isogeny prepare CONFIG_FILE
       isogeny deploy [STOW_ARGS]
TEMPLATE: Selmer template
CONTEXT: EDN context map
OUTPUT: output location")

(def general-opts
  [["-h" "--help" "Display usage information." :id ::+help?]
   ["-V" "--version" "Print the version of Isogeny." :id ::+version?]
   ["-v" "--verbose" "Print execution details." :id ::+verbose?]
   [nil, "--strict" "Throw an exception for missing values." :id ::+strict?]
   [nil, "--safe" "Do not alter existing files." :id ::+safe?]
   [nil, "--dry-run" "Run without writing anything." :id ::+dry-run?]])

(def render-opts
  [["-t" "--template TEMPLATE" "Templates to be rendered."
    :id ::template-files :multi true :default [] :update-fn conj]
   ["-c" "--context CONTEXT" "EDN context used to render template." :id ::context-file :default "-"]
   ["-d" "--context-default CONTEXT" "Default EDN context to fall back on." :id ::context-default-file]
   ["-C" "--context-override STRING" "Provide context override." :id ::context-override]
   ["-j" "--json" "Parse JSON context." :id ::json?]
   ["-a" "--add-tags TAGS_FILE" "File containing additional tag definitions." :id ::add-tags]
   [nil, "--deep-merge" "Deep-merge the context override." :id ::deep-merge?]
   ["-o" "--output OUTPUT" "Locations for output."
    :id ::outputs :multi true :default [] :update-fn conj]
   [nil "--standard" "Assume outputs to be template minus extension." :id ::standard?]
   [nil "--eval" "Allow #=() reader macros in EDN contexts." :id ::eval?]])

(def deploy-opts
  [[nil "--deploy-dir DIR" "The root dir of config files, --dir or -d in GNU Stow." :id ::deploy-dir]])

(def cli-opts
  (concat general-opts render-opts deploy-opts))

;; Add the {% env %} tag to read environment variables.
(selmer.parser/add-tag!
 :env (fn [args _] (System/getenv (first args))))

(defn pp-str
  [x]
  (with-out-str (pp/pprint x)))

(defn exit
  ([] (System/exit 0))
  ([n] (System/exit n)))

(defn help!
  []
  (let [general-opts-summary (->> general-opts (cli/parse-opts []) :summary)
        render-opts-summary (->> render-opts (cli/parse-opts []) :summary)
        deploy-opts-summary (->> deploy-opts (cli/parse-opts []) :summary)]
    (println usage "\n")
    (println "General options:") (println general-opts-summary) (println)
    (println "Render options:") (println render-opts-summary) (println)
    (println "Deploy options:") (println deploy-opts-summary) (println)))

(defn version!
  []
  (println "Isogeny 4.0.0"))

(defn throw-on-missing!
  "Causes an exception to be thrown when a missing value is encountered."
  []
  (when @verbose? (log/info "Throwing on missing values."))
  (s.util/set-missing-value-formatter!
   (fn [tag _]
     (-> "Missing value: "
         (str (or (:tag-value tag) (:tag-name tag)))
         Exception.
         throw))))

(defn add-tags!
  "Add additional tags to use when rendering the template."
  [add-tags]
  (when @verbose? (log/info "Loading tags from:" add-tags))
  (try (load-file add-tags)
       (catch Exception e
         (log/error "Exception thrown loading additional tags:")
         (throw e))))

(defn setup!
  [{::keys [+help? +version? +verbose? +strict? +safe? +dry-run?] :as options}]
  (when +help? (help!) (exit))
  (when +version? (version!) (exit))
  (when +verbose? (reset! verbose? true) (log/info "Options:" (pp-str options)))
  (when +strict? (throw-on-missing!))
  (when +safe? (reset! safe? true) (when @verbose? (log/info "Enabling safe mode.")))
  (when +dry-run? (reset! dry-run? true) (when @verbose? (log/info "Enabling dry-run mode."))))

(defn try-slurp!
  ([file] (try-slurp! file nil))
  ([file {:keys [throw?]}]
   (try (when @verbose? (log/info "Reading:" file))
        (slurp file)
        (catch Exception e
          (when @verbose? (log/warn "Exception reading file:" (.getMessage e)))
          (when throw? (throw e))))))

(defn -deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with -deep-merge maps)
    (last maps)))

(defn deep-merge
  "Like -deep-merge but ignores nil arguments."
  [& maps]
  (apply -deep-merge (filter some? maps)))

(defn throw-missing-ctxt
  "This function is called to throw an exception when the context cannot be determined."
  [& [opts]]
  (->> (pp-str opts)
       (vector "Neither context, nor default, nor override can be read.")
       (str/join "\n")
       (new Exception)
       throw))

(defn ->context
  ([ctxt] (->context ctxt nil nil nil))
  ([ctxt default] (->context ctxt default nil nil))
  ([ctxt default override] (->context ctxt default override nil))
  ([ctxt default override {::keys [json? deep-merge? eval?] :as opts}]
   (let [parse-fn (if json?
                    #(json/parse-string % true)
                    #(binding [*read-eval* (boolean eval?)]
                       (read-string %)))
         merge-fn (if deep-merge? deep-merge merge)
         parsed-override (when (some? override) (parse-fn override))]
     (-> ctxt
         (or default override (throw-missing-ctxt opts))
         parse-fn
         (merge-fn parsed-override)))))

(defn ->input
  [input]
  (if (= "-" input)
    *in*
    input))

(defn ->output
  [o]
  (condp = o
    "-" *out*
    nil *out*
    o))

(defn ->outputs
  [outputs]
  (when @verbose? (log/info "Outputs:" outputs))
  (if (empty? outputs)
    (repeat nil)
    (map ->output outputs)))

(defn ->standard
  [template-files]
  (map fs/strip-ext template-files))

(defn render
  [templates context context-default options]
  (let [{::keys [template-files context-override outputs standard?]} options
        merged-context (->context context context-default context-override options)
        fixed-outputs (if standard? (->standard template-files) (->outputs outputs))]
    (->> templates
         (map #(s.parser/render % merged-context))
         (map (fn [f c] {::file f ::content c}) fixed-outputs))))

(defn write-each!
  [{::keys [file content]}]
  (when @verbose? (log/info "Writing to file:" file))
  (if @dry-run?
    (log/warn "DRY-RUN, not writing to:" file)
    (if (and @safe? (fs/exists? file))
      (log/warn "Running safely so not writing to:" file)
      (spit (or file *out*) content))))

(defn write!
  [outputs]
  (let [output-list (doall (map write-each! outputs))]
    (when @verbose? (log/info "Wrote:" (count output-list) "files"))
    output-list))

(defn render!
  [_ {::keys [template-files context-file context-default-file add-tags]
      :as options}]
  (when @verbose? (log/info "Rendering:" (pp-str options)))
  (let [slurp-input! (fn [input throw?] (try-slurp! (->input input) {:throw? throw?}))
        templates (doall (map #(slurp-input! % true) template-files))
        context (slurp-input! context-file false)
        context-default (slurp-input! context-default-file false)]
    (when add-tags (add-tags! add-tags))
    (-> templates
        (render context context-default options)
        write!)))

(defn prepare
  ([f] (prepare f true))
  ([{::keys [file content]} resolve-host?]
   (let [template (str file ".template")
         host (if resolve-host?
                (or (.. java.net.InetAddress getLocalHost getHostName) "HOST")
                "HOST")
         context (str/join "." [file host "edn"])]
     (when @verbose?
       (log/info "File:" file)
       (log/info "Template:" template)
       (log/info "Context:" context))
     [{::file template ::content content}
      {::file context ::content "{}"}])))

(defn prepare!
  [configs _]
  (when @verbose? (log/info "Preparing:" configs))
  (->> configs
       (map (fn [f] {::file f ::content (try-slurp! f {:throw? true})}))
       (map prepare)
       (apply concat)
       write!))

(defn deploy!
  [args {::keys [deploy-dir]}]
  (cond
    @safe? (log/warn "Deploy cannot be run safely.")
    @dry-run? (log/info "Not deploying anything due to --dry-run.")
    :else (let [dir (if (some? deploy-dir) ["--dir" deploy-dir] [])
                v (if @verbose? ["-vv"] [])
                stow-args (concat v dir args)
                {:keys [exit out err] :as ret} (apply shell/sh "stow" stow-args)
                failed? (< 0 exit)]
            (when @verbose?
              (log/info "Stow return:" ret)
              (doall (for [line (str/split out #"\n")]
                       (when-not (empty? line)
                         (log/info line)))))
            (when (or failed? @verbose?)
              (doall (for [line (str/split err #"\n")]
                       (log/info line)))))))

(defn -main
  "CLI entry point. Dispatches to render, prepare, or deploy."
  [& args]
  (let [{[subcommand & subargs :as arguments] :arguments
         :keys [options errors]} (cli/parse-opts args cli-opts)]
    (when errors (log/error "Errors parsing CLI arguments:" errors) (exit 1))
    (setup! options)
    (when @verbose? (log/info "Arguments:" arguments))
    (case subcommand
      "render" (render! subargs options)
      "prepare" (prepare! subargs options)
      "deploy" (deploy! subargs options)
      (render! args options))))

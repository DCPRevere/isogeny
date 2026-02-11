(ns dcprevere.isogeny-test
  (:require [babashka.fs :as fs]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :as t]
            [cheshire.core :as json]
            [dcprevere.isogeny :as sut]
            [selmer.parser :as s.parser]
            [selmer.util :as s.util]))

(defn tmp-file
  [suffix]
  (str (fs/path (fs/temp-dir) (str "isogeny-test-" (java.util.UUID/randomUUID) suffix))))

(defn exit-ex
  "A replacement for sut/exit that throws instead of calling System/exit."
  [& [n]]
  (throw (ex-info "exit called" {::exit-code (or n 0)})))

(defmacro capture-out
  "Captures *out* even if the body throws. Returns [output-string thrown-or-nil]."
  [& body]
  `(let [sw# (java.io.StringWriter.)]
     (try (binding [*out* sw#] ~@body)
          [(str sw#) nil]
          (catch Exception e#
            [(str sw#) e#]))))

(defn delete-quietly [f]
  (when (and f (fs/exists? f))
    (fs/delete f)))

(defn reset-atoms-fixture [f]
  (reset! sut/verbose? false)
  (reset! sut/safe? false)
  (reset! sut/dry-run? false)
  (s.util/set-missing-value-formatter! s.util/default-missing-value-formatter)
  (f)
  (reset! sut/verbose? false)
  (reset! sut/safe? false)
  (reset! sut/dry-run? false)
  (s.util/set-missing-value-formatter! s.util/default-missing-value-formatter))

(t/use-fixtures :each reset-atoms-fixture)

(t/deftest -deep-merge-test
  (t/testing "Flat maps are merged like merge."
    (t/is (= {:a 1 :b 2} (#'sut/-deep-merge {:a 1} {:b 2}))))
  (t/testing "Nested maps are merged recursively."
    (t/is (= {:a {:b 1 :c 2}} (#'sut/-deep-merge {:a {:b 1}} {:a {:c 2}}))))
  (t/testing "Later values win for non-map leaves."
    (t/is (= {:a {:b 2}} (#'sut/-deep-merge {:a {:b 1}} {:a {:b 2}}))))
  (t/testing "Three-level nesting."
    (t/is (= {:a {:b {:c 1 :d 2}}}
             (#'sut/-deep-merge {:a {:b {:c 1}}} {:a {:b {:d 2}}}))))
  (t/testing "Non-map values are not merged."
    (t/is (= {:a [3 4]} (#'sut/-deep-merge {:a [1 2]} {:a [3 4]})))))

(t/deftest deep-merge-test
  (t/testing "Maps can be deep-merged."
    (let [m1 {:foo {:bar :bar :qux :qux}}
          m2 {:foo {:bar :baz}}
          expected {:foo {:bar :baz :qux :qux}}
          actual (sut/deep-merge m1 m2)]
      (t/is (= expected actual))))
  (t/testing "Nil maps are ignored."
    (let [m1 {:foo :bar}
          m2 nil
          expected {:foo :bar}
          actual (sut/deep-merge m1 m2)]
      (t/is (= expected actual))))
  (t/testing "Both nil returns nil."
    (t/is (nil? (sut/deep-merge nil nil))))
  (t/testing "Multiple nils are ignored."
    (t/is (= {:a 1} (sut/deep-merge nil {:a 1} nil))))
  (t/testing "Three maps deep-merged."
    (t/is (= {:a {:b 1 :c 2 :d 3}}
             (sut/deep-merge {:a {:b 1}} {:a {:c 2}} {:a {:d 3}})))))

(t/deftest ->outputs-test
  (t/testing "Proper outputs are unchanged."
    (let [o1 "foo" o2 "-" o3 nil
          expected [o1 *out* *out*]
          actual (sut/->outputs [o1 o2 o3])]
      (t/is (= expected actual))))
  (t/testing "Proper outputs are unchanged."
    (let [o "foo"
          expected [o]
          actual (sut/->outputs [o])]
      (t/is (= expected actual))))
  (t/testing "Hyphens redirect to *out*."
    (let [o "-"
          expected [*out*]
          actual (sut/->outputs [o])]
      (t/is (= expected actual))))
  (t/testing "Missing outputs redirect to *out*."
    (let [o nil
          expected [*out*]
          actual (sut/->outputs [o])]
      (t/is (= expected actual)))))

(t/deftest ->standard-test
  (t/testing "Extensions are stripped from template file paths."
    (let [template-files ["file.foo" "other.file.bar"]
          expected ["file" "other.file"]
          actual (sut/->standard template-files)]
      (t/is (= expected actual)))))

(t/deftest render-test
  (t/testing "The context will be preferred to its default."
    (let [templates ["{{foo}}"]
          context (str {:foo "bar"})
          context-default (str {:foo "baz"})
          output "output.txt"
          options {::sut/outputs [output]}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Default context will be used if the context is nil."
    (let [templates ["{{foo}}"]
          context nil
          context-default (str {:foo "bar"})
          output "output.txt"
          options {::sut/outputs [output]}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "A present context can be overridden."
    (let [templates ["{{foo}}\n{{fee}}"]
          context (str {:foo "baz" :fee "qux"})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/context-override (str {:foo "bar"})}
          expected [{::sut/file output ::sut/content "bar\nqux"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "A nil context can be overridden."
    (let [templates ["{{foo}}"]
          context nil
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/context-override (str {:foo "bar"})}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Interior values can be used in the template."
    (let [templates ["{{foo.bar}}"]
          context (str {:foo {:bar "baz"}})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output]}
          expected [{::sut/file output ::sut/content "baz"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "JSON can be used to provide context."
    (let [templates ["{{foo}}"]
          context (json/generate-string {:foo "baz"})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/json? true}
          expected [{::sut/file output ::sut/content "baz"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Contexts can be deep-merged."
    (let [templates ["{{foo.one}} {{foo.two}} {{foo.three}}"]
          context (str {:foo {:one "one" :two "two"}})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output]
                   ::sut/context-override (str {:foo {:two "two" :three "three"}})
                   ::sut/deep-merge? true}
          expected [{::sut/file output ::sut/content "one two three"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Reader eval macros are executed when eval? is true."
    (let [templates ["{{foo}}\n{{bar}}"]
          context "{:foo #=(+ 1 2) :bar \"baz\"}"
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/eval? true}
          expected [{::sut/file output ::sut/content "3\nbaz"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Reader eval macros throw when eval? is false."
    (t/is (thrown? Exception
                   (sut/render ["{{foo}}"]
                               "{:foo #=(+ 1 2)}"
                               nil
                               {::sut/outputs ["out"]}))))
  (t/testing "Bare forms are treated as data without eval."
    (let [templates ["{{foo}}"]
          context (str {:foo '(+ 1 2)})
          output "output.txt"
          options {::sut/outputs [output]}
          result (sut/render templates context nil options)]
      (t/is (= "(+ 1 2)" (::sut/content (first result))))))
  (t/testing "Standard option will override outputs."
    (let [template-files ["output.template"]
          templates ["{{foo}}"]
          context (str {:foo "bar"})
          context-default nil
          output "output"
          options {::sut/template-files template-files
                   ::sut/outputs ["incorrect.file"]
                   ::sut/deep-merge? true
                   ::sut/standard? true}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual)))))

(t/deftest ->input-test
  (t/testing "Hyphen returns *in*."
    (t/is (= *in* (sut/->input "-"))))
  (t/testing "A path is returned as-is."
    (t/is (= "/some/path" (sut/->input "/some/path"))))
  (t/testing "Empty string is returned as-is."
    (t/is (= "" (sut/->input "")))))

(t/deftest ->output-test
  (t/testing "Hyphen returns *out*."
    (t/is (= *out* (sut/->output "-"))))
  (t/testing "Nil returns *out*."
    (t/is (= *out* (sut/->output nil))))
  (t/testing "A path is returned as-is."
    (t/is (= "/some/path" (sut/->output "/some/path")))))

(t/deftest ->context-test
  (t/testing "Parses EDN context."
    (t/is (= {:foo "bar"} (sut/->context (str {:foo "bar"})))))
  (t/testing "Falls back to default when context is nil."
    (t/is (= {:foo "bar"} (sut/->context nil (str {:foo "bar"})))))
  (t/testing "Context is preferred over default."
    (t/is (= {:foo "bar"} (sut/->context (str {:foo "bar"}) (str {:foo "baz"})))))
  (t/testing "Override is applied on top of context."
    (t/is (= {:foo "override" :bar "baz"}
             (sut/->context (str {:foo "bar" :bar "baz"})
                            nil
                            (str {:foo "override"})))))
  (t/testing "Override alone works when context and default are nil."
    (t/is (= {:foo "bar"} (sut/->context nil nil (str {:foo "bar"})))))
  (t/testing "JSON context is parsed when json? is set."
    (t/is (= {:foo "bar"}
             (sut/->context (json/generate-string {:foo "bar"})
                            nil nil {::sut/json? true}))))
  (t/testing "JSON override is parsed when json? is set."
    (t/is (= {:foo "override"}
             (sut/->context (json/generate-string {:foo "bar"})
                            nil
                            (json/generate-string {:foo "override"})
                            {::sut/json? true}))))
  (t/testing "Deep-merge option merges override recursively."
    (t/is (= {:a {:b 1 :c 2}}
             (sut/->context (str {:a {:b 1}})
                            nil
                            (str {:a {:c 2}})
                            {::sut/deep-merge? true}))))
  (t/testing "Shallow merge replaces nested maps by default."
    (t/is (= {:a {:c 2}}
             (sut/->context (str {:a {:b 1}})
                            nil
                            (str {:a {:c 2}})))))
  (t/testing "Throws when all sources are nil."
    (t/is (thrown? Exception (sut/->context nil nil nil)))))

(t/deftest throw-missing-ctxt-test
  (t/testing "Throws an exception."
    (t/is (thrown? Exception (sut/throw-missing-ctxt))))
  (t/testing "Exception message contains the expected text."
    (t/is (thrown-with-msg? Exception
                           #"Neither context, nor default, nor override"
                           (sut/throw-missing-ctxt)))))

(t/deftest try-slurp-test
  (t/testing "Reads an existing file."
    (let [tmp (tmp-file ".txt")]
      (try
        (spit tmp "hello")
        (t/is (= "hello" (sut/try-slurp! tmp)))
        (finally (delete-quietly tmp)))))
  (t/testing "Returns nil for missing file when throw? is false."
    (t/is (nil? (sut/try-slurp! "/nonexistent/path/file.txt"))))
  (t/testing "Returns nil for missing file with no options."
    (t/is (nil? (sut/try-slurp! "/nonexistent/path/file.txt" nil))))
  (t/testing "Throws for missing file when throw? is true."
    (t/is (thrown? Exception
                   (sut/try-slurp! "/nonexistent/path/file.txt"
                                   {:throw? true})))))

;; Regression: callers pass {:throw? true} with an unqualified key.

(t/deftest try-slurp!-unqualified-throw-test
  (t/testing "try-slurp! with unqualified :throw? throws for missing file."
    (t/is (thrown? Exception
                   (sut/try-slurp! "/nonexistent/path/file.txt"
                                   {:throw? true})))))

(t/deftest write-each-test
  (t/testing "Writes content to a file."
    (let [tmp (tmp-file ".txt")]
      (try
        (sut/write-each! {::sut/file tmp ::sut/content "written"})
        (t/is (= "written" (slurp tmp)))
        (finally (delete-quietly tmp)))))
  (t/testing "Dry-run does not write."
    (let [tmp (tmp-file ".txt")]
      (try
        (spit tmp "original")
        (reset! sut/dry-run? true)
        (sut/write-each! {::sut/file tmp ::sut/content "overwritten"})
        (t/is (= "original" (slurp tmp)))
        (finally (delete-quietly tmp)))))
  (t/testing "Safe mode does not overwrite existing files."
    (let [tmp (tmp-file ".txt")]
      (try
        (reset! sut/dry-run? false)
        (spit tmp "original")
        (reset! sut/safe? true)
        (sut/write-each! {::sut/file tmp ::sut/content "overwritten"})
        (t/is (= "original" (slurp tmp)))
        (finally (delete-quietly tmp)))))
  (t/testing "Safe mode writes to new files."
    (let [tmp (tmp-file ".txt")]
      (try
        (reset! sut/dry-run? false)
        (reset! sut/safe? true)
        (sut/write-each! {::sut/file tmp ::sut/content "new"})
        (t/is (= "new" (slurp tmp)))
        (finally (delete-quietly tmp))))))

(t/deftest write-test
  (t/testing "Writes multiple files."
    (let [t1 (tmp-file ".txt")
          t2 (tmp-file ".txt")]
      (try
        (sut/write! [{::sut/file t1 ::sut/content "one"}
                     {::sut/file t2 ::sut/content "two"}])
        (t/is (= "one" (slurp t1)))
        (t/is (= "two" (slurp t2)))
        (finally
          (delete-quietly t1)
          (delete-quietly t2))))))

(t/deftest prepare-test
  (t/testing "Config files may be prepared for use with Isogeny."
    (let [file "config" content "content"
          expected [{::sut/file "config.template" ::sut/content content}
                    {::sut/file "config.HOST.edn" ::sut/content "{}"}]
          actual (sut/prepare {::sut/file file ::sut/content content} false)]
      (t/is (= expected actual))))
  (t/testing "Host is resolved when resolve-host? is true."
    (let [host (.. java.net.InetAddress getLocalHost getHostName)
          result (sut/prepare {::sut/file "cfg" ::sut/content "x"} true)]
      (t/is (= (str "cfg." host ".edn") (::sut/file (second result))))))
  (t/testing "Template gets .template extension."
    (let [result (sut/prepare {::sut/file "my.conf" ::sut/content "data"} false)]
      (t/is (= "my.conf.template" (::sut/file (first result))))))
  (t/testing "Context file gets empty map content."
    (let [result (sut/prepare {::sut/file "my.conf" ::sut/content "data"} false)]
      (t/is (= "{}" (::sut/content (second result)))))))

(t/deftest render-multi-template-test
  (t/testing "Multiple templates are rendered with the same context."
    (let [templates ["{{a}}" "{{b}}"]
          context (str {:a "one" :b "two"})
          options {::sut/outputs ["o1" "o2"]}
          result (sut/render templates context nil options)]
      (t/is (= [{::sut/file "o1" ::sut/content "one"}
                {::sut/file "o2" ::sut/content "two"}]
               result)))))

(t/deftest render-selmer-tags-test
  (t/testing "Selmer for-loop tag works."
    (let [templates ["{% for x in items %}{{x}} {% endfor %}"]
          context (str {:items ["a" "b" "c"]})
          options {::sut/outputs ["out"]}
          result (sut/render templates context nil options)]
      (t/is (= "a b c " (::sut/content (first result))))))
  (t/testing "Selmer if tag works."
    (let [templates ["{% if show %}visible{% endif %}"]
          context (str {:show true})
          options {::sut/outputs ["out"]}
          result (sut/render templates context nil options)]
      (t/is (= "visible" (::sut/content (first result))))))
  (t/testing "Selmer if-else tag works."
    (let [templates ["{% if show %}yes{% else %}no{% endif %}"]
          context (str {:show false})
          options {::sut/outputs ["out"]}
          result (sut/render templates context nil options)]
      (t/is (= "no" (::sut/content (first result)))))))

(t/deftest render!-integration-test
  (t/testing "Renders a template file to an output file."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "Hello, {{name}}!")
        (spit ctx (str {:name "world"}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs [out]})
        (t/is (= "Hello, world!" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out)))))
  (t/testing "Renders with default context when primary is missing."
    (let [tpl (tmp-file ".template")
          dfl (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{val}}")
        (spit dfl (str {:val "fallback"}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file "/nonexistent/file.edn"
                          ::sut/context-default-file dfl
                          ::sut/outputs [out]})
        (t/is (= "fallback" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly dfl)
          (delete-quietly out))))))

(t/deftest pp-str-test
  (t/testing "Pretty-prints a value to a string."
    (t/is (string? (sut/pp-str {:a 1}))))
  (t/testing "Contains the printed value."
    (t/is (re-find #":a 1" (sut/pp-str {:a 1})))))

;; ---- setup! ----

(t/deftest setup!-verbose-test
  (t/testing "Verbose flag sets the atom."
    (sut/setup! {::sut/+verbose? true})
    (t/is (true? @sut/verbose?))))

(t/deftest setup!-safe-test
  (t/testing "Safe flag sets the atom."
    (sut/setup! {::sut/+safe? true})
    (t/is (true? @sut/safe?))))

(t/deftest setup!-dry-run-test
  (t/testing "Dry-run flag sets the atom."
    (sut/setup! {::sut/+dry-run? true})
    (t/is (true? @sut/dry-run?))))

(t/deftest setup!-strict-test
  (t/testing "Strict flag causes missing values to throw during render."
    (sut/setup! {::sut/+strict? true})
    (t/is (thrown? Exception
                   (s.parser/render "{{missing}}" {})))))

(t/deftest setup!-help-test
  (t/testing "Help flag prints help and calls exit."
    (with-redefs [sut/exit exit-ex]
      (let [[output ex] (capture-out (sut/setup! {::sut/+help? true}))]
        (t/is (some? ex))
        (t/is (= 0 (::exit-code (ex-data ex))))
        (t/is (str/includes? output "USAGE"))))))

(t/deftest setup!-version-test
  (t/testing "Version flag prints version and calls exit."
    (with-redefs [sut/exit exit-ex]
      (let [[output ex] (capture-out (sut/setup! {::sut/+version? true}))]
        (t/is (some? ex))
        (t/is (= 0 (::exit-code (ex-data ex))))
        (t/is (str/includes? output "Isogeny"))))))

(t/deftest setup!-noop-test
  (t/testing "No flags leaves atoms unchanged."
    (sut/setup! {})
    (t/is (false? @sut/verbose?))
    (t/is (false? @sut/safe?))
    (t/is (false? @sut/dry-run?))))

;; ---- throw-on-missing! integration ----

(t/deftest throw-on-missing!-render-test
  (t/testing "After throw-on-missing!, rendering with missing variables throws."
    (sut/throw-on-missing!)
    (t/is (thrown? Exception
                   (s.parser/render "{{missing}}" {}))))
  (t/testing "Rendering with all variables present succeeds."
    (sut/throw-on-missing!)
    (t/is (= "hello" (s.parser/render "{{x}}" {:x "hello"})))))

;; ---- env tag ----

(t/deftest env-tag-test
  (t/testing "The env tag reads environment variables."
    (let [user (System/getenv "HOME")]
      (t/is (= user (s.parser/render "{% env HOME %}" {})))))
  (t/testing "The env tag returns nil string for missing env var."
    (t/is (= "" (s.parser/render "{% env ISOGENY_NONEXISTENT_VAR_12345 %}" {})))))

;; ---- add-tags! ----

(t/deftest add-tags!-test
  (t/testing "Custom tags from file are loaded."
    (let [tag-file (tmp-file ".clj")]
      (try
        (spit tag-file "(selmer.parser/add-tag! :testtag42 (fn [_ _] \"loaded\"))")
        (sut/add-tags! tag-file)
        (t/is (= "loaded" (s.parser/render "{% testtag42 %}" {})))
        (finally (delete-quietly tag-file)))))
  (t/testing "Throws for nonexistent tag file."
    (t/is (thrown? Exception (sut/add-tags! "/nonexistent/tags.clj")))))

;; ---- help! / version! ----

(t/deftest help!-test
  (t/testing "Help output contains usage and option sections."
    (let [output (with-out-str (sut/help!))]
      (t/is (str/includes? output "USAGE"))
      (t/is (str/includes? output "General options"))
      (t/is (str/includes? output "Render options"))
      (t/is (str/includes? output "--template"))
      (t/is (str/includes? output "--context"))
      (t/is (str/includes? output "--help")))))

(t/deftest version!-test
  (t/testing "Version output contains version string."
    (let [output (with-out-str (sut/version!))]
      (t/is (str/includes? output "Isogeny"))
      (t/is (re-find #"\d+\.\d+\.\d+" output)))))

;; Regression: usage and help! must mention deploy.

(t/deftest usage-mentions-deploy-test
  (t/testing "Usage string mentions deploy subcommand."
    (t/is (str/includes? sut/usage "deploy"))))

(t/deftest help!-shows-deploy-options-test
  (t/testing "help! output includes deploy options."
    (let [output (with-out-str (sut/help!))]
      (t/is (str/includes? output "deploy-dir")))))

;; ---- ->outputs edge cases ----

(t/deftest ->outputs-empty-test
  (t/testing "Empty outputs returns an infinite lazy seq of nil."
    (let [result (sut/->outputs [])]
      (t/is (nil? (first result)))
      (t/is (nil? (second result)))
      (t/is (= [nil nil nil] (take 3 result))))))

;; ---- ->standard edge cases ----

(t/deftest ->standard-edge-cases-test
  (t/testing "File with no extension."
    (t/is (= ["Makefile"] (sut/->standard ["Makefile"]))))
  (t/testing "File with multiple dots."
    (t/is (= ["config.yaml"] (sut/->standard ["config.yaml.template"]))))
  (t/testing "Empty list."
    (t/is (= [] (sut/->standard [])))))

;; ---- write-each! to stdout ----

(t/deftest write-each!-stdout-test
  (t/testing "Nil file writes to *out*."
    (let [output (with-out-str
                   (sut/write-each! {::sut/file nil ::sut/content "to-stdout"}))]
      (t/is (= "to-stdout" output)))))

;; ---- deploy! ----

(t/deftest deploy!-safe-test
  (t/testing "Deploy warns and does nothing in safe mode."
    (reset! sut/safe? true)
    (let [shell-called (atom false)]
      (with-redefs [shell/sh (fn [& _] (reset! shell-called true) {:exit 0 :out "" :err ""})]
        (sut/deploy! ["pkg"] {})
        (t/is (false? @shell-called))))))

(t/deftest deploy!-dry-run-test
  (t/testing "Deploy does nothing in dry-run mode."
    (reset! sut/dry-run? true)
    (let [shell-called (atom false)]
      (with-redefs [shell/sh (fn [& _] (reset! shell-called true) {:exit 0 :out "" :err ""})]
        (sut/deploy! ["pkg"] {})
        (t/is (false? @shell-called))))))

(t/deftest deploy!-calls-stow-test
  (t/testing "Deploy calls stow with the correct arguments."
    (let [captured-args (atom nil)]
      (with-redefs [shell/sh (fn [& args] (reset! captured-args (vec args)) {:exit 0 :out "" :err ""})]
        (sut/deploy! ["pkg1" "pkg2"] {})
        (t/is (= ["stow" "pkg1" "pkg2"] @captured-args))))))

(t/deftest deploy!-with-deploy-dir-test
  (t/testing "Deploy passes --dir to stow."
    (let [captured-args (atom nil)]
      (with-redefs [shell/sh (fn [& args] (reset! captured-args (vec args)) {:exit 0 :out "" :err ""})]
        (sut/deploy! ["pkg"] {::sut/deploy-dir "/home/user/dots"})
        (t/is (= ["stow" "--dir" "/home/user/dots" "pkg"] @captured-args))))))

(t/deftest deploy!-verbose-test
  (t/testing "Deploy passes -vv to stow when verbose."
    (reset! sut/verbose? true)
    (let [captured-args (atom nil)]
      (with-redefs [shell/sh (fn [& args] (reset! captured-args (vec args)) {:exit 0 :out "" :err ""})]
        (sut/deploy! ["pkg"] {})
        (t/is (= ["stow" "-vv" "pkg"] @captured-args))))))

;; ---- prepare! integration ----

(t/deftest prepare!-integration-test
  (t/testing "Prepare reads a config and writes template + context files."
    (let [cfg (tmp-file ".conf")
          host (.. java.net.InetAddress getLocalHost getHostName)
          expected-tpl (str cfg ".template")
          expected-ctx (str cfg "." host ".edn")]
      (try
        (spit cfg "original content")
        (sut/prepare! [cfg] {})
        (t/is (= "original content" (slurp expected-tpl)))
        (t/is (= "{}" (slurp expected-ctx)))
        (finally
          (delete-quietly cfg)
          (delete-quietly expected-tpl)
          (delete-quietly expected-ctx)))))
  (t/testing "Prepare throws for nonexistent config."
    (t/is (thrown? Exception (sut/prepare! ["/nonexistent/file"] {})))))

;; ---- render! integration: standard mode ----

(t/deftest render!-standard-mode-test
  (t/testing "Standard mode outputs to template path minus extension."
    (let [dir (str (fs/create-temp-dir))
          tpl (str (fs/path dir "config.template"))
          ctx (tmp-file ".edn")
          expected-out (str (fs/path dir "config"))]
      (try
        (spit tpl "{{val}}")
        (spit ctx (str {:val "standard-works"}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs []
                          ::sut/standard? true})
        (t/is (= "standard-works" (slurp expected-out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly expected-out)
          (delete-quietly ctx)
          (delete-quietly dir))))))

;; ---- render! integration: JSON context file ----

(t/deftest render!-json-context-test
  (t/testing "Renders using a JSON context file."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".json")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{name}} is {{age}}")
        (spit ctx (json/generate-string {:name "Alice" :age 30}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs [out]
                          ::sut/json? true})
        (t/is (= "Alice is 30" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

;; ---- render! integration: context override ----

(t/deftest render!-context-override-test
  (t/testing "Context override replaces values from file."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{x}} {{y}}")
        (spit ctx (str {:x "original" :y "kept"}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/context-override (str {:x "replaced"})
                          ::sut/outputs [out]})
        (t/is (= "replaced kept" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

;; ---- render! integration: deep-merge override ----

(t/deftest render!-deep-merge-override-test
  (t/testing "Deep-merge preserves nested keys from context."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{a.b}} {{a.c}}")
        (spit ctx (str {:a {:b "base" :c "base"}}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/context-override (str {:a {:c "overridden"}})
                          ::sut/deep-merge? true
                          ::sut/outputs [out]})
        (t/is (= "base overridden" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

;; ---- render! integration: add-tags ----

(t/deftest render!-add-tags-test
  (t/testing "Custom tags from file are available during render."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          tag (tmp-file ".clj")
          out (tmp-file ".txt")]
      (try
        (spit tpl "result: {% mytag99 %}")
        (spit ctx (str {:x 1}))
        (spit tag "(selmer.parser/add-tag! :mytag99 (fn [_ _] \"custom-value\"))")
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/add-tags tag
                          ::sut/outputs [out]})
        (t/is (= "result: custom-value" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly tag)
          (delete-quietly out))))))

;; ---- render! integration: strict mode ----

(t/deftest render!-strict-test
  (t/testing "Render throws on missing values after throw-on-missing!."
    (sut/throw-on-missing!)
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")]
      (try
        (spit tpl "{{present}} {{missing}}")
        (spit ctx (str {:present "here"}))
        (t/is (thrown? Exception
                       (sut/render! nil {::sut/template-files [tpl]
                                         ::sut/context-file ctx
                                         ::sut/outputs [(tmp-file ".txt")]})))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx))))))

;; ---- render! integration: dry-run ----

(t/deftest render!-dry-run-test
  (t/testing "Dry-run does not create the output file."
    (reset! sut/dry-run? true)
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{x}}")
        (spit ctx (str {:x "val"}))
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs [out]})
        (t/is (not (fs/exists? out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx))))))

;; ---- render! integration: safe mode ----

(t/deftest render!-safe-test
  (t/testing "Safe mode does not overwrite existing output."
    (reset! sut/safe? true)
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{x}}")
        (spit ctx (str {:x "new"}))
        (spit out "old")
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs [out]})
        (t/is (= "old" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

;; ---- render! integration: multi-template with files ----

(t/deftest render!-multi-template-test
  (t/testing "Multiple templates are rendered to separate outputs."
    (let [tpl1 (tmp-file ".template")
          tpl2 (tmp-file ".template")
          ctx (tmp-file ".edn")
          out1 (tmp-file ".txt")
          out2 (tmp-file ".txt")]
      (try
        (spit tpl1 "first: {{a}}")
        (spit tpl2 "second: {{b}}")
        (spit ctx (str {:a "one" :b "two"}))
        (sut/render! nil {::sut/template-files [tpl1 tpl2]
                          ::sut/context-file ctx
                          ::sut/outputs [out1 out2]})
        (t/is (= "first: one" (slurp out1)))
        (t/is (= "second: two" (slurp out2)))
        (finally
          (delete-quietly tpl1)
          (delete-quietly tpl2)
          (delete-quietly ctx)
          (delete-quietly out1)
          (delete-quietly out2))))))

;; ---- render! integration: missing template throws ----

(t/deftest render!-missing-template-test
  (t/testing "Missing template file throws."
    (let [ctx (tmp-file ".edn")]
      (try
        (spit ctx (str {:x 1}))
        (t/is (thrown? Exception
                       (sut/render! nil {::sut/template-files ["/nonexistent/tpl"]
                                         ::sut/context-file ctx
                                         ::sut/outputs [(tmp-file ".txt")]})))
        (finally
          (delete-quietly ctx))))))

;; ---- render! integration: Clojure eval in context ----

(t/deftest render!-eval-context-test
  (t/testing "Reader eval macros in EDN context are executed with --eval."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{x}}")
        (spit ctx "{:x #=(+ 10 20)}")
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs [out]
                          ::sut/eval? true})
        (t/is (= "30" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out)))))
  (t/testing "Reader eval macros in EDN context throw without --eval."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")]
      (try
        (spit tpl "{{x}}")
        (spit ctx "{:x #=(+ 10 20)}")
        (t/is (thrown? Exception
                       (sut/render! nil {::sut/template-files [tpl]
                                         ::sut/context-file ctx
                                         ::sut/outputs [(tmp-file ".txt")]})))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx))))))

;; ---- render! integration: env tag in template ----

(t/deftest render!-env-tag-test
  (t/testing "The env tag renders environment variables in templates."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")
          home (System/getenv "HOME")]
      (try
        (spit tpl "home: {% env HOME %}")
        (spit ctx "{}")
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/outputs [out]})
        (t/is (= (str "home: " home) (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

;; ---- -main integration ----

(t/deftest -main-render-subcommand-test
  (t/testing "The render subcommand renders a template."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{greeting}}")
        (spit ctx (str {:greeting "hi"}))
        (with-redefs [sut/exit exit-ex]
          (sut/-main "render" "-t" tpl "-c" ctx "-o" out))
        (t/is (= "hi" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

(t/deftest -main-default-subcommand-test
  (t/testing "No subcommand defaults to render."
    (let [tpl (tmp-file ".template")
          ctx (tmp-file ".edn")
          out (tmp-file ".txt")]
      (try
        (spit tpl "{{x}}")
        (spit ctx (str {:x "default"}))
        (with-redefs [sut/exit exit-ex]
          (sut/-main "-t" tpl "-c" ctx "-o" out))
        (t/is (= "default" (slurp out)))
        (finally
          (delete-quietly tpl)
          (delete-quietly ctx)
          (delete-quietly out))))))

(t/deftest -main-prepare-subcommand-test
  (t/testing "The prepare subcommand creates template and context files."
    (let [cfg (tmp-file ".conf")
          host (.. java.net.InetAddress getLocalHost getHostName)
          expected-tpl (str cfg ".template")
          expected-ctx (str cfg "." host ".edn")]
      (try
        (spit cfg "cfg content")
        (with-redefs [sut/exit exit-ex]
          (sut/-main "prepare" cfg))
        (t/is (= "cfg content" (slurp expected-tpl)))
        (t/is (= "{}" (slurp expected-ctx)))
        (finally
          (delete-quietly cfg)
          (delete-quietly expected-tpl)
          (delete-quietly expected-ctx))))))

(t/deftest -main-deploy-subcommand-test
  (t/testing "The deploy subcommand calls stow."
    (let [captured-args (atom nil)]
      (with-redefs [sut/exit exit-ex
                    shell/sh (fn [& args] (reset! captured-args (vec args)) {:exit 0 :out "" :err ""})]
        (sut/-main "deploy" "--deploy-dir" "/dots" "pkg1")
        (t/is (= ["stow" "--dir" "/dots" "pkg1"] @captured-args))))))

(t/deftest -main-version-test
  (t/testing "-main with --version prints version."
    (with-redefs [sut/exit exit-ex]
      (let [[output ex] (capture-out (sut/-main "--version"))]
        (t/is (some? ex))
        (t/is (= 0 (::exit-code (ex-data ex))))
        (t/is (str/includes? output "Isogeny"))))))

(t/deftest -main-help-test
  (t/testing "-main with --help prints help."
    (with-redefs [sut/exit exit-ex]
      (let [[output ex] (capture-out (sut/-main "--help"))]
        (t/is (some? ex))
        (t/is (= 0 (::exit-code (ex-data ex))))
        (t/is (str/includes? output "USAGE"))))))

;; ---- example files end-to-end ----

(def examples-dir "examples")

(t/deftest example-edn-render-test
  (t/testing "Renders alacritty template with desktop EDN context."
    (let [tpl (str (fs/path examples-dir "alacritty.toml.template"))
          ctx (str (fs/path examples-dir "alacritty.toml.desktop.edn"))
          tag (str (fs/path examples-dir "custom-tag.clj"))
          out (tmp-file ".toml")
          shell (System/getenv "SHELL")]
      (try
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/add-tags tag
                          ::sut/outputs [out]})
        (let [content (slurp out)]
          (t/is (str/includes? content "size = 14"))
          (t/is (str/includes? content "JetBrains Mono"))
          (t/is (str/includes? content "#1e1e2e"))
          (t/is (str/includes? content (str "program = \"" shell "\"")))
          (t/is (str/includes? content "Generated by Isogeny")))
        (finally
          (delete-quietly out))))))

(t/deftest example-json-render-test
  (t/testing "Renders alacritty template with desktop JSON context."
    (let [tpl (str (fs/path examples-dir "alacritty.toml.template"))
          ctx (str (fs/path examples-dir "alacritty.toml.desktop.json"))
          tag (str (fs/path examples-dir "custom-tag.clj"))
          out (tmp-file ".toml")]
      (try
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/add-tags tag
                          ::sut/outputs [out]
                          ::sut/json? true})
        (let [content (slurp out)]
          (t/is (str/includes? content "size = 14"))
          (t/is (str/includes? content "JetBrains Mono"))
          (t/is (str/includes? content "#1e1e2e")))
        (finally
          (delete-quietly out))))))

(t/deftest example-default-context-test
  (t/testing "Falls back to default context when specific is missing."
    (let [tpl (str (fs/path examples-dir "alacritty.toml.template"))
          ctx "/nonexistent/context.edn"
          dfl (str (fs/path examples-dir "alacritty.toml.default.edn"))
          tag (str (fs/path examples-dir "custom-tag.clj"))
          out (tmp-file ".toml")]
      (try
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/context-default-file dfl
                          ::sut/add-tags tag
                          ::sut/outputs [out]})
        (let [content (slurp out)]
          (t/is (str/includes? content "size = 12"))
          (t/is (str/includes? content "monospace")))
        (finally
          (delete-quietly out))))))

(t/deftest example-multi-template-test
  (t/testing "Multiple templates render with the same context."
    (let [tpl1 (str (fs/path examples-dir "alacritty.toml.template"))
          tpl2 (str (fs/path examples-dir "colors.css.template"))
          ctx (str (fs/path examples-dir "alacritty.toml.desktop.edn"))
          tag (str (fs/path examples-dir "custom-tag.clj"))
          out1 (tmp-file ".toml")
          out2 (tmp-file ".css")]
      (try
        (sut/render! nil {::sut/template-files [tpl1 tpl2]
                          ::sut/context-file ctx
                          ::sut/add-tags tag
                          ::sut/outputs [out1 out2]})
        (t/is (str/includes? (slurp out1) "size = 14"))
        (t/is (str/includes? (slurp out2) "--bg: #1e1e2e"))
        (finally
          (delete-quietly out1)
          (delete-quietly out2))))))

(t/deftest example-override-test
  (t/testing "Deep-merge override changes font size but preserves font family."
    (let [tpl (str (fs/path examples-dir "alacritty.toml.template"))
          ctx (str (fs/path examples-dir "alacritty.toml.desktop.edn"))
          tag (str (fs/path examples-dir "custom-tag.clj"))
          out (tmp-file ".toml")]
      (try
        (sut/render! nil {::sut/template-files [tpl]
                          ::sut/context-file ctx
                          ::sut/add-tags tag
                          ::sut/context-override (str {:font {:size 18}})
                          ::sut/deep-merge? true
                          ::sut/outputs [out]})
        (let [content (slurp out)]
          (t/is (str/includes? content "size = 18"))
          (t/is (str/includes? content "JetBrains Mono")))
        (finally
          (delete-quietly out))))))

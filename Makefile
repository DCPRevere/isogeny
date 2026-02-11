VERSION := 4.0.0
JAR := target/isogeny-$(VERSION).jar

.PHONY: test uberjar native clean

test:
	clojure -X:test

uberjar:
	clojure -T:build ci

native: $(JAR)
	native-image \
		--features=clj_easy.graal_build_time.InitClojureClasses \
		--initialize-at-build-time=com.fasterxml.jackson \
		--no-fallback \
		-H:ConfigurationFileDirectories=graalvm/ \
		-jar $(JAR) \
		-o isogeny-native

$(JAR): src/dcprevere/isogeny.clj deps.edn
	clojure -T:build ci

# Regenerate graalvm/ configs using the tracing agent.
# Requires a JDK with native-image-agent installed.
graalvm-config: $(JAR)
	java -agentlib:native-image-agent=config-merge-dir=graalvm/ \
		-jar $(JAR) --version
	java -agentlib:native-image-agent=config-merge-dir=graalvm/ \
		-jar $(JAR) render \
		-t examples/alacritty.toml.template \
		-c examples/alacritty.toml.desktop.edn \
		-a examples/custom-tag.clj
	java -agentlib:native-image-agent=config-merge-dir=graalvm/ \
		-jar $(JAR) render \
		-t examples/alacritty.toml.template \
		-c examples/alacritty.toml.desktop.json \
		--json

clean:
	rm -rf target isogeny-native

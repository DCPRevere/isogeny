# Usage

Isogeny generates configuration files from templates and context data.

## Subcommands

### render (default)

Render a template with a context to produce output.

```sh
isogeny render -t TEMPLATE -c CONTEXT -o OUTPUT
```

If no subcommand is given, `render` is assumed.

### prepare

Create starter template and context files from an existing config.

```sh
isogeny prepare config.toml
# produces: config.toml.template, config.toml.HOSTNAME.edn
```

### deploy

Thin wrapper around GNU Stow for deploying rendered configs.

```sh
isogeny deploy --deploy-dir ~/.dotfiles alacritty emacs
```

## Contexts

A context provides the values substituted into a template. EDN and JSON are supported.

EDN:
```clojure
{:font {:size 14
        :family "JetBrains Mono"}
 :colors {:background "#1e1e2e"}}
```

JSON (requires `--json`):
```json
{
  "font": {"size": 14, "family": "JetBrains Mono"},
  "colors": {"background": "#1e1e2e"}
}
```

### Default context

A fallback context used when the primary context file is missing.

```sh
isogeny -t config.template \
    -c "config.$(hostname).edn" \
    -d config.default.edn \
    -o config
```

### Context override

Override specific values from the command line.

```sh
isogeny -t config.template \
    -c config.edn \
    -C '{:font {:size 18}}' \
    -o config
```

By default, overrides use a shallow merge. Use `--deep-merge` to merge nested maps recursively:

```sh
# Without --deep-merge: {:font {:size 18}} replaces the entire :font map
# With --deep-merge: {:font {:size 18}} merges into {:font {:size 14 :family "..."}}
isogeny -t config.template \
    -c config.edn \
    -C '{:font {:size 18}}' \
    --deep-merge \
    -o config
```

### Code execution

With the `--eval` flag, EDN contexts can contain `#=()` reader macros:

```clojure
{:user #=(java.lang.System/getenv "USER")
 :columns #=(* 80 2)}
```

Without `--eval`, `#=()` throws an exception. Classes must be fully qualified.

## Templates

Templates use [Selmer](https://github.com/yogthos/Selmer) syntax.

Variables: `{{ variable }}`, `{{ nested.key }}`

Tags: `{% if condition %}`, `{% for x in items %}`, `{% env VARNAME %}`

Custom tags can be loaded from a Clojure file with `-a`:

```sh
isogeny -t config.template -c config.edn -a tags.clj -o config
```

## Flags

| Flag | Effect |
|---|---|
| `--standard` | Output path = template path minus extension |
| `--strict` | Throw on missing template variables |
| `--safe` | Do not overwrite existing files |
| `--dry-run` | Run without writing output |
| `--eval` | Allow `#=()` reader macros in EDN contexts |
| `--deep-merge` | Recursively merge context overrides |
| `--json` | Parse context as JSON instead of EDN |
| `--verbose` | Print execution details |

## Multi-template

Render multiple templates with one context. Order of `-t` and `-o` must match.

```sh
isogeny -c shared-context.edn \
    -t alacritty.toml.template -o alacritty.toml \
    -t colors.css.template -o colors.css
```

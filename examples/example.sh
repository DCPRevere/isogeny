#!/usr/bin/env bash

# Basic render with machine-specific context
isogeny render \
    -t examples/alacritty.toml.template \
    -c examples/alacritty.toml.desktop.edn \
    -a examples/custom-tag.clj \
    -o examples/alacritty.toml

# Render with default context fallback
isogeny render \
    -t examples/alacritty.toml.template \
    -c "examples/alacritty.toml.$(hostname).edn" \
    -d examples/alacritty.toml.default.edn \
    -a examples/custom-tag.clj \
    -o examples/alacritty.toml

# Override font size on the command line with deep-merge
isogeny render \
    -t examples/alacritty.toml.template \
    -c examples/alacritty.toml.desktop.edn \
    -a examples/custom-tag.clj \
    -C '{:font {:size 18}}' \
    --deep-merge \
    -o examples/alacritty.toml

# Multi-template: generate both alacritty config and CSS variables
isogeny render \
    -t examples/alacritty.toml.template \
    -o examples/alacritty.toml \
    -t examples/colors.css.template \
    -o examples/colors.css \
    -c examples/alacritty.toml.desktop.edn \
    -a examples/custom-tag.clj

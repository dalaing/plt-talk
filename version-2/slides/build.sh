#! /usr/bin/env bash
nix-shell -I . --command "pdflatex --shell-escape slides.tex"

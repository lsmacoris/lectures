#!/usr/bin/env bash
# Recompila as tabelas LaTeX (standalone) e gera os SVGs usados em aula_fearp_v2.qmd
# Uso: bash build.sh            (todas)
#      bash build.sh T03_baseline (só uma)
cd "$(dirname "$0")"
PDFLATEX=/c/Users/lucas/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex
files=${1:+$1.tex}
for f in ${files:-T*.tex A*.tex}; do
  b=${f%.tex}
  "$PDFLATEX" -interaction=nonstopmode -halt-on-error "$f" >/dev/null 2>&1 \
    && pdftocairo -svg "$b.pdf" "$b.svg" && echo "OK   $b" || echo "FAIL $b (ver $b.log)"
done
rm -f *.aux

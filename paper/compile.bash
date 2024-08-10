#!/bin/bash

# from https://tex.stackexchange.com/a/458352

BASE="${1:-main}"  # BASE will be assigned 'main' if arg not set or null.
pdflatex -shell-escape $BASE.tex
if [ $? -ne 0 ]; then
    echo "Compilation error. Check log."
    exit 1
fi
bibtex $BASE
pdflatex -shell-escape $BASE.tex
pdflatex -shell-escape $BASE.tex
exit 0

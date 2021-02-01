#!/usr/bin/env bash

set -e

bg="none"

echo "\\documentclass[bg=$bg,10pt,a4paper,twocolumn,openany,nomultitoc,nodeprecatedcode]{book}

\usepackage[layout=true]{dnd}

\usepackage[english]{babel}
\usepackage[utf8]{inputenc}
\usepackage[singlelinecheck=false]{caption}
\usepackage{lipsum}
\usepackage{listings}
\usepackage{shortvrb}
\usepackage{stfloats}

\pagenumbering{gobble}

\usepackage{amsmath}
\usepackage{tikz}
\\newcommand{\\ExtLink}{
    \\includegraphics[height=5pt]{ext_link.png}
}

\\titleformat{\\chapter}
    {\\DndFontChapter} % format
    {}
    {0pt}
    {\\DndContour} % before-code

\\fancyhf{}

\\begin{document}
"

cat /dev/stdin

echo "
\\end{document}
"

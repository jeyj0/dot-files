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
    {}
    {\\DndContour} % before-code

\\fancyhf{}

\\begin{document}
"

is_in_table=0
REGEX_TABLE="^\|"

echo "$(cat $1)
" \
    | while read -r line; do {
          if echo "$line" | grep -P -q $REGEX_TABLE; then
              if test 1 -eq $is_in_table; then
                  echo "$line";
              else
                  is_in_table=1;
                  n=$(expr $(echo "$line" | sed -r 's/\|/\n/g' | wc -l) - 2)
                  table_format="X"
                  if test $n -eq 2; then
                      table_format="lX"
                  elif test $n -eq 3; then
                      table_format="XXX"
                  elif test $n -gt 3; then
                      table_format="X$(printf '%0.sc' $(seq 1 $(expr $n - 2)))r"
                  fi
                  echo "\\begin{DndTable}[]{$table_format}
$line";
              fi
          else
              if test 1 -eq $is_in_table; then
                  is_in_table=0;
                  echo "\\end{DndTable}
$line";
              else
                  echo "$line";
              fi
          fi
      }; done \
    | sed -E 's/^#\+TITLE: (.*)$/\\chapter{\1}\n\n/' \
    | sed -E 's/^#\+.*$//' \
    | sed -E 's/^- (.*) :: (.*)$/\\paragraph{\1} \2/' \
    | sed -E 's/^\*\ (.*)$/\\section{\1}/' \
    | sed -E 's/^\*\* (.*) :area:$/\\DndArea{\1}/' \
    | sed -E 's/^\*\*\ (.*)$/\\subsection{\1}/' \
    | sed -E 's/^\*\*\* (.*) :subarea:$/\\DndSubArea{\1}/' \
    | sed -E 's/^\*\*\*\ (.*)$/\\subsubsection{\1}/' \
    | sed -E 's/^\*\*\*\*\ (.*)$/\\subparagraph{\1}/' \
    | sed -E 's/\[\[[^]]+\]\[([^]]+)\]\]/\\emph{\\textcolor{rulered}{\1\\ExtLink}}/' \
    | sed -E 's/\[\[([^]]+)\]\]/\\emph{\\textcolor{rulered}{\1}}/' \
    | sed -E '/^\|/s/ ([^|]+) \|/ \1\&/g' \
    | sed -E 's/^\|[-+]+\|/\\hline \\\\/' \
    | sed -E 's/^\|(.*)&/\1 \\\\/' \
    | sed -E 's/([^a-zA-Z0-9])\*([^\ ][^*]*[^\ ])\*([^a-zA-Z0-9])/\1\\textbf{\2}\3/g' \
    | sed -E 's/([^a-zA-Z0-9])\/([^\ ][^\/]*[^\ ])\/([^a-zA-Z0-9])/\1\\textit{\2}\3/g' \
    | sed -E 's/([^a-zA-Z0-9])~([^\ ][^~]*[^\ ])~([^a-zA-Z0-9])/\1\\texttt{\2}\3/g' \

echo "
\\end{document}
"

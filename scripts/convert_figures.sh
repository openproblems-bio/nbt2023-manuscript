#!/bin/bash

for pdf in figures/figure1/*.pdf ; do
  png="${pdf%.*}.png"
  if [[ "$pdf" -nt "$png" || "scripts/convert_figures.sh" -nt "$png" ]]; then
    echo "Converting $pdf"
    convert -verbose -density 300 "$pdf" "$png"
    echo 
  fi
done

for pdf in figures/figure2/*.pdf ; do
  png="${pdf%.*}.png"
  if [[ "$pdf" -nt "$png" || "scripts/convert_figures.sh" -nt "$png" ]]; then
    echo "Converting $pdf"
    convert -verbose -density 300 "$pdf" "$png"
    echo 
  fi
done

for pdf in figures/supnote2_figures/*/*.pdf ; do
  png="${pdf%.*}.png"
  if [[ "$pdf" -nt "$png" || "scripts/convert_figures.sh" -nt "$png" ]]; then
    echo "Converting $pdf"
    convert -verbose -density 300 "$pdf" "$png"
    echo 
  fi
done
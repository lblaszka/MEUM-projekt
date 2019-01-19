#!/bin/bash
cd ./output
for f in *\ *; do mv "$f" "${f// /_}"; done
cd ../

mkdir -p ./pdfs
mkdir -p ./pdfs/output
mkdir -p ./pdfs/output/ts


array=(`ls ./output/*.svg`)

len=${#array[*]}

i=0
while [ $i -lt $len ]; do
    echo "$i: ${array[$i]}"
    inkscape -D -z --file=${array[$i]} --export-pdf=./pdfs/${array[$i]::-4}.pdf --export-latex
    let i++
done

exit

array=(`ls ./output/ts/*.svg`)

len=${#array[*]}

i=0
while [ $i -lt $len ]; do
    echo "$i: ${array[$i]}"
    inkscape -D -z --file=${array[$i]} --export-pdf=./pdfs/${array[$i]::-4}.pdf --export-latex
    let i++
done

#inkscape -D -z --file=err_density_common.svg --export-pdf=image.pdf --export-latex

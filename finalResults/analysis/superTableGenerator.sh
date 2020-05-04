#! /usr/bin/bash
pushd ../
for analysis in $(find -wholename "*run*analysis" -type d)
do
    echo $file;pushd $analysis; source ./tableGenerator.sh ;popd
done
popd


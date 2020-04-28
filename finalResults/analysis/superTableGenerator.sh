#! /usr/bin/bash
pushd ../
for file in $(find -wholename "*run*tableGenerator.sh")
do
    echo $file;pushd $(dirname $file); source $(basename $file);popd
done
popd


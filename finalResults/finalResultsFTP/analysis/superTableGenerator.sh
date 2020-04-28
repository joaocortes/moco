#! /usr/bin/bash
pushd ../
for run in run*
do
    for file in $(find $run -name "*tableGenerator.sh")
    do
	pushd $(dirname $file); source $(basename $file);popd
    done
done

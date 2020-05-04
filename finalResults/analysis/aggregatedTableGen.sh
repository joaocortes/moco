#! /usr/bin/bash
> aggregatedTable.out
for run in ../run*
do
    for file in $(find $run -name "*table.out")
    do
	cat $file >> aggregatedTable.out
    done
done
column -t aggregatedTable.out > temp.out;
cp temp.out aggregatedTable.out;
cat aggregatedTable.out

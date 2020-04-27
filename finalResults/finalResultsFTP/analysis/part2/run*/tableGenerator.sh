#! /usr/bin/bash
> table.out
pushd ../output/
> ../analysis/table.out
for file in solver*; do ../analysis/fileFilter.awk $file >> ../analysis/table.out;done
popd


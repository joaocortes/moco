#! /usr/bin/bash
> table.out
pushd ../output/
> ../analysis/table.out
for file in solver*; do ../analysis/fileFilter.awk $file >> ../analysis/table.out;done
echo "most abstract one"
popd
column -t  table.out > nada.out;
cp nada.out table.out
rm nada.out
cat table.out




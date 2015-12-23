#!/bin/bash

for i in {1..10000}
do
    index=$(printf "%06d" $i)
    dist/build/LangtonAnt/LangtonAnt 4 100 $i > evol/evol$index.txt
done
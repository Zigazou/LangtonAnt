#!/bin/bash

for i in {1..200}
do
    index=$(printf "%03d" $i)
    dist/build/LangtonAnt/LangtonAnt $i $i > evol/evol$index.txt
done
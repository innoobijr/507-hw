#!/bin/bash

for i in {0..3} ; do
    dot $i.dot -Tsvg > $i.svg
done

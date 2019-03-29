#!/usr/bin/env bash

function base {
    workfolder=output/$1
    shift
    red -fW $workfolder --stdout -i input.json -o $workfolder/output.json $@ ./predicate.sh {}
}

mkdir -p output

base "bin-lines"
base "bin-hdd" -F json -T hdd
base "bin-flat" -F json -T flat
base "bin-graph" -F json -T graph

base "ddmin-lines" -R ddmin
base "ddmin-hdd"   -R ddmin  -F json -T hdd
base "ddmin-flat"  -R ddmin  -F json -T flat
base "ddmin-graph" -R ddmin  -F json -T graph

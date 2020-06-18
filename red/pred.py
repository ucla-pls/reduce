#!/usr/bin/env python3
"""This script represents a simple predicate which support having dependencies.
Any monotonic predicate can be represented as a list of accepted sets, where we
require at least one of the sets is fully contained in the inputs.

> pred.py --deps depfile.csv --accept 1,12 nodes.txt

The script will return 0 if all checks are satisfied, 1 if no accepted set
is completely covered, and 2 the dependencies are not satisfied. The dependency
check takes precedence.

The dependency file is a csv file with a "from", "to" columns.
"""
import sys

from collections import defaultdict
from csv import DictReader
from pathlib import Path

def main(nodefile, depfile, accepted):
    nodes = {e.strip() for e in nodefile.read_text().split('\n') if e}
    if depfile:
        with open(str(depfile)) as filep:
            edges = list(find_missing_dependencies(nodes, DictReader(filep)))
        if edges:
            print(f"Found missing dependencies:", file=sys.stderr)
            for edge in edges:
                print(f"    {edge[0]} -> {edge[1]}", file=sys.stderr)
            sys.exit(2)

    for aset in accepted:
        if aset <= nodes:
            print(f"Nodes contains {aset}", file=sys.stderr)
            sys.exit(0)

    print(f"No accepted set is contained in nodes.", file=sys.stderr)
    sys.exit(1)

def find_missing_dependencies(nodes, depends):
    for dep in depends:
        edge_from, edge_to = dep["from"], dep["to"]
        if edge_from in nodes and not edge_to in nodes:
            yield edge_from, edge_to

def parse_args():
    import argparse

    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter
        )
    parser.add_argument(
        "--deps", metavar="FILE", dest="depfile", type=Path,
        help="a csv file containing edges")
    parser.add_argument(
        "--accept", dest="accepted", type=lambda s: set(s.split(',')), action='append',
        help="a comma separated list of nodes that constitutes an accepted set.")
    parser.add_argument(
        "--accept-file", dest="accept_file", type=Path,
        help="a line separated file containing a list of comma accepted sets.")
    parser.add_argument(
        "nodefile", metavar="NODE_FILE", type=Path,
        help="a newlines separated file of nodes")

    args = parser.parse_args()
    
    if args.accept_file:
        args.accepted.extend([
            set(s.strip.split(',')) 
            for s in args.accept_file.read_text().split("\n") 
            if s
            ])

    return args

if __name__ == "__main__":
    ARGS = parse_args()
    main(nodefile=ARGS.nodefile, depfile=ARGS.depfile, accepted=ARGS.accepted)

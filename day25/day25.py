import math
import igraph

with open('day25/input.txt', 'r') as f:
    d = f.read()

data = {}
for line in d.splitlines():
    a,b = line.split(':')
    data[a] = b.split()

graph = igraph.Graph.ListDict(data)
subgraphs = graph.mincut()

if len(subgraphs) != 2:
    raise RuntimeError('the minimum cut doesn\'t cut into 2 subgraphs!')

print('Part 1:', len(subgraphs[0]) * len(subgraphs[1]))

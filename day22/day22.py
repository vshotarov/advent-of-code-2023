from copy import deepcopy
from collections import defaultdict
import heapq

with open("day22/input.txt","r") as f:
    d = f.read()

bricks = []
for l in d.splitlines():
    end1,end2 = [[int(y) for y in x.split(",")] for x in l.split("~")]
    (x1,y1,z1),(x2,y2,z2) = end1,end2

    as_2D_set = set()
    for x in range(min(x1,x2),max(x1,x2)+1):
        for y in range(min(y1,y2),max(y1,y2)+1):
            as_2D_set.add((x,y))

    bricks.append(((min(x1,x2), min(y1,y2), min(z1,z2)),
                   (max(x1,x2), max(y1,y2), max(z1,z2)),
                   frozenset(as_2D_set)))

bricks = sorted(bricks, key=lambda p: p[0][2])

def fall(bricks):
    fallen = []
    reverse_fallen = []
    for b in bricks:
        b_min,b_max,b_set = b
        new_z = 1
        for (other_min,other_max,other_set) in reverse_fallen:
            if other_set.intersection(b_set):
                new_z = max(new_z, other_max[2]+1)
        delta = b_min[2] - new_z
        b_prime = ((b[0][0],b[0][1],b[0][2]-delta),(b[1][0],b[1][1],b[1][2]-delta),b[2])
        fallen.append(b_prime)
        reverse_fallen.insert(0,b_prime)
    return fallen

fallen = fall(bricks)
set_fallen = set(fallen)

dp = defaultdict(set)
for i,brick in enumerate(fallen):
    for other_brick in fallen[(i+1):]:
        if other_brick[0][2] == brick[1][2] + 1 and brick[2].intersection(other_brick[2]):
            dp[other_brick].add(brick)

pt1,pt2 = 0,0
for i,brick in enumerate(fallen):
    if set([brick]) not in dp.values():
        pt1 += 1
    else:
        removed = set([brick])
        for b2 in fallen:
            if dp[b2] and not dp[b2].difference(removed):
                removed.add(b2)
        pt2 += len(removed)-1

print("Part 1:", pt1)
print("Part 2:", pt2)

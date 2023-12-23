from collections import defaultdict
import math

with open("day20/input.txt","r") as f:
    d=f.read()

mods = {}
inputs = defaultdict(set)
for line in d.splitlines():
    a,b = line.split(" -> ")
    if a != "broadcaster":
        x = [a[0],a[1:],b.split(", "),0]
        mods[a[1:]] = x
    else:
        x = [a[0],a[0:],b.split(", "),0]
        mods[a[0:]] = x

    for bb in x[2]:
        inputs[bb].add(x[1])

for k,v in inputs.items():
    if k not in mods:
        mods[k] = ["output", k, [], 0]

for k,m in mods.items():
    if m[0] == "&":
        m[-1] = {k:0 for k in inputs[m[1]]}

l,h=0,0
cl_high_signals = {}
def press(i):
    explore = [("button","broadcaster",0)]
    global l,h

    while explore:
        src,dst,sig = explore.pop(0)

        if sig == 0:
            l += 1
        else:
            h += 1

        #print(src, "-", "low" if not sig else "high", "->", dst)

        typ,name,new_dests,state = mods[dst]

        if dst == "cl" and sig == 1 and src not in cl_high_signals:
            cl_high_signals[src] = i

        new_sig = sig

        if typ == "%":
            if sig == 1:
                continue

            if state == 0:
                mods[dst][3] = 1
                new_sig = 1
            else:
                mods[dst][3] = 0
                new_sig = 0
        elif typ == "&":
            state[src] = sig
            if all([x==1 for x in state.values()]):
                new_sig = 0
            else:
                new_sig = 1

        explore += [(name,d,new_sig) for d in new_dests]

for i in range(10000):
    if i == 1000:
        print("Part 1:",l*h)
    press(i+1)

print("Part 2:",math.lcm(*cl_high_signals.values()))

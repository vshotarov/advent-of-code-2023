import heapq

with open("day17/input.txt","r") as f:
    d=f.read()

size = len(d.splitlines())
grid = {(x,y):int(v) for y,row in enumerate(d.splitlines()) for x,v in enumerate(row)}

explore = []
heapq.heappush(explore, (0,0,0,1,0))
heapq.heappush(explore, (0,0,0,0,1))
best = float('inf')
seen = set()

while explore:
    hl,x,y,dx,dy = heapq.heappop(explore)

    if (x,y,dx,dy) in seen:
        continue

    if (x,y) == (size-1,size-1):
        best = min(best, hl)
        continue

    seen.add((x,y,dx,dy))

    for ddx,ddy in [(-dy,dx),(dy,-dx)]:
        nhl = hl
        for i in range(10):
            n = x+ddx*(i+1),y+ddy*(i+1)
            if n not in grid:
                break
            nhl += grid[n]
            if i >= 3:
                heapq.heappush(explore, (nhl,n[0],n[1],ddx,ddy))

print(best)

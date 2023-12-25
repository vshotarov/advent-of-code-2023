from sympy import solve
from sympy.abc import x,y,z,i,j,k,s,r,t

PS = [[206273907288897, 404536114337943, 197510451330134],
      [318919383845607, 260745469021671, 223155534318195],
      [379055259398812, 255495760772511, 396757430832289]]
VS = [[-18,6,92],[-78,62,75],[-179,-18,-373]]
X = [w[0] for w in PS]
Y = [w[1] for w in PS]
Z = [w[2] for w in PS]
VX = [w[0] for w in VS]
VY = [w[1] for w in VS]
VZ = [w[2] for w in VS]

sol = solve([
    x+s*i-(X[0]+s*VX[0]),
    y+s*j-(Y[0]+s*VY[0]),
    z+s*k-(Z[0]+s*VZ[0]),
    x+r*i-(X[1]+r*VX[1]),
    y+r*j-(Y[1]+r*VY[1]),
    z+r*k-(Z[1]+r*VZ[1]),
    x+t*i-(X[2]+t*VX[2]),
    y+t*j-(Y[2]+t*VY[2]),
    z+t*k-(Z[2]+t*VZ[2]),
])[0]

print(sol[x]+sol[y]+sol[z])

import sys

#filename = sys.argv[1]
filename = sys.argv[1]
f = open(filename, "r")
f = f.read()
map = []
line = []
for x in f:
    if x!='\n':
        line.append(x)
    else:
        map.append(line)
        line = []

N = len(map)
M = len(map[0])

frontier = []
level = dict()
tsiou = (-1,-1)
airports = dict()
airport = []
fwlia = (-2,-2)
for i in range(N):
    for j in range(M):
        if map[i][j] == 'W':
            frontier.append((i, j))
            level[(i, j)] = 0
        if map[i][j] == 'S':
            tsiou = (i, j)
        if map[i][j] == 'A':
            airports[i,j] = 1
            airport.append((i,j))
        if map[i][j] == 'T':
            fwlia = (i,j)
i = 1
flag = 0
air = 0
while frontier:
    next = []
    for u in frontier:
        if u[0] < N-1 and (u[0]+1, u[1]) not in level and map[u[0]+1][u[1]] != 'X': #down
            if(flag==1):
                air = air + 1
            if(air==5):
                for k in airport:
                    level[(k[0], k[1])] = i
                    next.append((k[0], k[1]))
            if(((u[0]+1, u[1]) in airports) and flag==0):
                flag=1
            level[(u[0]+1, u[1])] = i
            next.append((u[0]+1, u[1]))
        if u[1] > 0 and (u[0], u[1]-1) not in level and map[u[0]][u[1]-1] != 'X': #left
            if(flag==1):
                air = air + 1
            if(air==5):
                for k in airport:
                    level[(k[0], k[1])] = i
                    next.append((k[0], k[1]))
            if(((u[0], u[1]-1) in airports) and flag==0):
                flag=1
            level[(u[0], u[1]-1)] = i
            next.append((u[0], u[1]-1))
        if u[1] < M-1 and (u[0], u[1]+1) not in level and map[u[0]][u[1]+1] != 'X': #right
            if(flag==1):
                air = air + 1
            if(air==5):
                for k in airport:
                    level[(k[0], k[1])] = i
                    next.append((k[0], k[1]))
            if(((u[0], u[1]+1) in airports) and flag==0):
                flag=1
            level[(u[0], u[1]+1)] = i
            next.append((u[0], u[1]+1))
        if u[0] > 0 and (u[0]-1, u[1]) not in level and map[u[0]-1][u[1]] != 'X': #up
            if(flag==1):
                air = air + 1
            if(air==5):
                for k in airport:
                    level[(k[0], k[1])] = i
                    next.append((k[0], k[1]))
            if(((u[0]-1, u[1]) in airports) and flag==0):
                flag=1
            level[(u[0]-1, u[1])] = i
            next.append((u[0]-1, u[1]))
    frontier = list(next)
    if(frontier and flag==0):
        i += 1
    if(flag):
        for k in airport:
            level[(k[0], k[1])] = i
            next.append((k[0], k[1]))
        flag = 0
        frontier = list(next)
        i = i + 5 - air

print(level)

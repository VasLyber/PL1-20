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
            airport.append((i,j))
        if map[i][j] == 'T':
            fwlia = (i,j)
i = 1
flag = 0
air = 0
flag2 = 0
frontier2 = []
while (frontier or frontier2):
    next = []
    next1 = []
    if(i%2==0):
        if(flag==1 and frontier):
            air = air + 1
        if(air==6 and frontier):
            for k in airport:
                level[(k[0], k[1])] = i
                next1.append((k[0], k[1]))
            frontier2 = list(next1)
            flag = 0
            air = 0
            flag2 = 1
        for u in frontier:
            if u[0] < N-1 and (u[0]+1, u[1]) not in level and map[u[0]+1][u[1]] != 'X': #down

                if(((u[0]+1, u[1]) in airport) and flag==0):
                    flag=1
                    airport.remove((u[0]+1, u[1]))
                if(((u[0], u[1]+1) in airport) and flag==1):
                    airport.remove((u[0], u[1]+1))                    
                level[(u[0]+1, u[1])] = i
                next.append((u[0]+1, u[1]))
            if u[1] > 0 and (u[0], u[1]-1) not in level and map[u[0]][u[1]-1] != 'X': #left
                if(((u[0], u[1]-1) in airport) and flag==0):
                    flag=1
                    airport.remove((u[0], u[1]-1))
                if(((u[0], u[1]+1) in airport) and flag==1):
                    airport.remove((u[0], u[1]+1))
                level[(u[0], u[1]-1)] = i
                next.append((u[0], u[1]-1))
            if u[1] < M-1 and (u[0], u[1]+1) not in level and map[u[0]][u[1]+1] != 'X': #right
                if(((u[0], u[1]+1) in airport) and flag==0):
                    flag=1
                    airport.remove((u[0], u[1]+1))
                if(((u[0], u[1]+1) in airport) and flag==1):
                    airport.remove((u[0], u[1]+1))
                level[(u[0], u[1]+1)] = i
                next.append((u[0], u[1]+1))
            if u[0] > 0 and (u[0]-1, u[1]) not in level and map[u[0]-1][u[1]] != 'X': #up
                if(((u[0]-1, u[1]) in airport) and flag==0):
                    flag=1
                    airport.remove((u[0]-1, u[1]))
                if(((u[0], u[1]+1) in airport) and flag==1):
                    airport.remove((u[0], u[1]+1))
                level[(u[0]-1, u[1])] = i
                next.append((u[0]-1, u[1]))
        frontier = list(next)

        if(frontier):
            i += 1
            continue
        if(flag):
            i = i + 5 - air
            for k in airport:
                level[(k[0], k[1])] = i
                next.append((k[0], k[1]))
            flag = 0
            flag2 = 1
            frontier2 = list(next)
            i += 1
            continue
        i += 1
    if(i%2==1):
        if(flag2==1):
            for u in frontier2:
                if u[0] < N-1 and (u[0]+1, u[1]) not in level and map[u[0]+1][u[1]] != 'X': #down
                    level[(u[0]+1, u[1])] = i
                    next.append((u[0]+1, u[1]))
                if u[1] > 0 and (u[0], u[1]-1) not in level and map[u[0]][u[1]-1] != 'X': #left
                    level[(u[0], u[1]-1)] = i
                    next.append((u[0], u[1]-1))
                if u[1] < M-1 and (u[0], u[1]+1) not in level and map[u[0]][u[1]+1] != 'X': #right
                    level[(u[0], u[1]+1)] = i
                    next.append((u[0], u[1]+1))
                if u[0] > 0 and (u[0]-1, u[1]) not in level and map[u[0]-1][u[1]] != 'X': #up
                    level[(u[0]-1, u[1])] = i
                    next.append((u[0]-1, u[1]))
            frontier2 = list(next)
            i += 1
        else:
            if(flag==1):
                air = air + 1
            if(air==6):
                for k in airport:
                    level[(k[0], k[1])] = i
                    next1.append((k[0], k[1]))
                frontier2 = list(next1)
                flag = 0
                air = 0
                flag2 = 1
            i += 1


flood_time = level
parent = {tsiou: (-1, -1)}
level = {tsiou: 0}
frontier = [tsiou]
i = 1


while frontier:
    next = []
    for pos in frontier:

        down = (pos[0]+1, pos[1])
        left = (pos[0], pos[1]-1)
        right = (pos[0], pos[1]+1)
        up = (pos[0]-1, pos[1])
        if (down not in level and down[0] < N and map[down[0]][down[1]] != 'X' and
            (flood_time[down] > i)):
            level[down] = i
            parent[down] = pos
            next.append(down)
        if (left not in level and left[1] >= 0 and map[left[0]][left[1]] != 'X' and
            (flood_time[left] > i)):
            level[left] = i
            parent[left] = pos
            next.append(left)
        if (right not in level and right[1] < M and map[right[0]][right[1]] != 'X' and
            (flood_time[right] > i)):
            level[right] = i
            parent[right] = pos
            next.append(right)
        if (up not in level and up[0] >= 0 and map[up[0]][up[1]] != 'X' and
            (flood_time[up] > i)):
            level[up] = i
            parent[up] = pos
            next.append(up)
    frontier = list(next)
    i += 1

x = 0
ans = ''
if(fwlia not in parent):
    print("IMPOSSIBLE")
else:
    while parent[fwlia] != (-1, -1):
        if parent[fwlia][0] == fwlia[0]-1:
            ans = 'D' + ans
            fwlia = parent[fwlia]
            x+=1
        elif parent[fwlia][0] == fwlia[0]+1:
            ans = 'U' + ans
            fwlia = parent[fwlia]
            x+=1
        elif parent[fwlia][1] == fwlia[1]-1:
            ans = 'R' + ans
            fwlia = parent[fwlia]
            x+=1
        elif parent[fwlia][1] == fwlia[1]+1:
            ans = 'L' + ans
            fwlia = parent[fwlia]
            x+=1
    print(x)
    print(ans)

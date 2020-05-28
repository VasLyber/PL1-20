import sys

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

# map is a 2d list

N = len(map)
M = len(map[0])
frontier = []
level = dict()
cat = (-1, -1)
for i in range(N):
    for j in range(M):
        if map[i][j] == 'W':
            frontier.append((i, j))
            level[(i, j)] = 0
        if map[i][j] == 'A':
            cat = (i, j)

i = 1

while frontier:
    next = []
    for u in frontier:
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


    frontier = list(next)
    i += 1
    # print("f")
    # print(frontier)
    # print("n")
    # print(next)
    # print("lev")
    # print(len(level))


print(frontier)
print("~~")
print(next)
print("~")
print(level)
print("~")


flood_time = level

for i in range(N):
    for j in range(M):
        if (i, j) not in flood_time and map[i][j] != 'X':
            flood_time[(i, j)] = 'infinity'

# safetime = dict()
parent = {cat: (-1, -1)}
level = {cat: 0}
max_time = -1
target = (-1, -1)
frontier = [cat]
i = 1

while frontier:
    next = []
    for pos in frontier:
        if flood_time[pos] == 'infinity':
            max_time = 'infinity'
            target = pos
        elif flood_time[pos] > max_time + 1:
            max_time = flood_time[pos]-1
            target = pos

        down = (pos[0]+1, pos[1])
        left = (pos[0], pos[1]-1)
        right = (pos[0], pos[1]+1)
        up = (pos[0]-1, pos[1])
        if (down not in level and down[0] < N and map[down[0]][down[1]] != 'X' and
            (flood_time[down] == 'infinity' or flood_time[down] > i) and max_time != 'infinity'):
            level[down] = i
            parent[down] = pos
            next.append(down)
        if (left not in level and left[1] >= 0 and map[left[0]][left[1]] != 'X' and
            (flood_time[left] == 'infinity' or flood_time[left] > i)):
            level[left] = i
            parent[left] = pos
            next.append(left)
        if (right not in level and right[1] < M and map[right[0]][right[1]] != 'X' and
            (flood_time[right] == 'infinity' or flood_time[right] > i) and max_time != 'infinity'):
            level[right] = i
            parent[right] = pos
            next.append(right)
        if (up not in level and up[0] >= 0 and map[up[0]][up[1]] != 'X' and
            (flood_time[up] =='infinity' or flood_time[up] > i)):
            level[up] = i
            parent[up] = pos
            next.append(up)
    frontier = list(next)
    i += 1

print(parent)

print(max_time)
if target == cat:
    print('stay')
else:
    ans = ''
    while parent[target] != (-1, -1):
        print(target)
        if parent[target][0] == target[0]-1:
            ans = 'D' + ans
            target = parent[target]
        elif parent[target][0] == target[0]+1:
            ans = 'U' + ans
            target = parent[target]
        elif parent[target][1] == target[1]-1:
            ans = 'R' + ans
            target = parent[target]
        elif parent[target][1] == target[1]+1:
            ans = 'L' + ans
            target = parent[target]
    print(ans)

import sys

#filename = sys.argv[1]
filename = "input1.txt"
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
airports = []
fwlia = (-2,-2)
for i in range(N):
    for j in range(M):
        if map[i][j] == 'W':
            frontier.append((i, j))
            level[(i, j)] = 0
        if map[i][j] == 'S':
            tsiou = (i, j)
        if map[i][j] == 'A':
            airports.append((i,j))
        if map[i][j] == 'T':
            fwlia = (i,j)

i = 1


while frontier:
    next = []
    for u in frontier:
        if u[0] < N-1 and (u[0]+1, u[1]) not in level and map[u[0]+1][u[1]] != 'X': #down
            level[(u[0]+1, u[1])] = i
            next.append((u[0]+1, u[1]))
            if map[u[0]+1][u[1]] != 'A':
                airports
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
    i += 2
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
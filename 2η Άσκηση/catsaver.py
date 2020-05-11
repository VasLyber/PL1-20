#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import collections

def process(f):
        hartis1 = f.read().split()
                #N, K = map(int, f.readline().split())
                #C = list(map(int, f.readline().split()))
        #print (len(hartis1),len(hartis1[1]))
        vasikia(hartis1)
	
def vasikia(hartis1):
        gatto=[]
        aqua=collections.deque()
        grammes=len(hartis1)
        stiles= len(hartis1[1])
        hartis2 = [[0 for x in range(stiles)] for y in range(grammes)] 
        for i in range(grammes):
                for j in range(stiles):
                        if hartis1[i][j]=='W':
                                aqua.append(i)
                                aqua.append(j)
                                hartis2[i][j]=0
                        elif hartis1[i][j]=='A':
                                gatto.append(i)
                                gatto.append(j)
                                gatto.append(-1)
                                gatto.append(0)
                                hartis2[i][j]=-3
                        elif hartis1[i][j]=='.':
                                hartis2[i][j]=-1
                        elif hartis1[i][j]=='X': 	
                                hartis2[i][j]=-2
        hartis2=plimmira(hartis2,aqua,grammes,stiles)
        #print (hartis2)
        foreversafe=1
        mesa1=0
        stay=1
        best_time=-1
        time=0
        koutaki=0
        i=gatto[0]
        j=gatto[1]
        if hartis2[i][j]>0:
                foreversafe=0
                best_time=hartis2[i][j]-1
                hartis2[i][j]=0
        posastoixeiamesa=0
        b=0
        best_i=i
        best_j=j
        if foreversafe==1:	
                while 1:
                        added=0
                        posastoixeiamesa=len(gatto)-b
                        for a in range (posastoixeiamesa//4):
                                i=gatto[b]
                                j=gatto[b+1]
                                if ((i+1 <grammes) and (hartis2[i+1][j]==-1)): #down
                                        added=added+1
                                        gatto.append(i+1)
                                        gatto.append(j)
                                        gatto.append(b)
                                        gatto.append(1)
                                        hartis2[i+1][j]=-5
                                if((j-1>=0) and (hartis2[i][j-1]==-1)): #left
                                        added=added+1
                                        gatto.append(i)
                                        gatto.append(j-1)
                                        gatto.append(b)
                                        gatto.append(2)
                                        if(i<= best_i) and (j-1 <=best_j):
                                            koutaki=b+(posastoixeiamesa//4-a+added-1)*4
                                            stay=0
                                            best_i=i
                                            best_j=j-1
                                        hartis2[i][j-1]=-5
                                if((j+1<stiles)and (hartis2[i][j+1]==-1)): #right
                                        added=added+1
                                        gatto.append(i)
                                        gatto.append(j+1)
                                        gatto.append(b)
                                        gatto.append(3)
                                        hartis2[i][j+1]=-5
                                if((i-1>=0) and (hartis2[i-1][j]==-1)): #up
                                        added=added+1
                                        gatto.append(i-1)
                                        gatto.append(j)
                                        gatto.append(b)
                                        gatto.append(4)
                                        if(i-1<= best_i ):
                                                koutaki=b+(posastoixeiamesa//4-a+added-1)*4
                                                stay=0
                                                best_i=i-1
                                                best_j=j
                                        hartis2[i-1][j]=-5
                                b=b+4
                        if(posastoixeiamesa==0):
                                break 
        else:
                while 1:
                        added=0
                        posastoixeiamesa=len(gatto)-b
                        """
                        if posastoixeiamesa>0:
                                i=gatto[b]
                                j=gatto[b+1]
                        """
                        for a in range (posastoixeiamesa//4):
                                i=gatto[b]
                                j=gatto[b+1]
                                if ((i+1 <grammes) and (hartis2[i+1][j]>time)): #down
                                        added=added+1
                                        gatto.append(i+1)
                                        gatto.append(j)
                                        gatto.append(b)
                                        gatto.append(1)
                                        if (best_time<hartis2[i+1][j]):
                                            best_time=hartis2[i+1][j]-1
                                            koutaki=b+(posastoixeiamesa//4-a+added-1)*4
                                            stay=0
                                            mesa1=1
                                            best_i=i+1
                                            best_j=j
                                        hartis2[i+1][j]=-5
                                if((j-1>=0) and (hartis2[i][j-1]>time)): #left
                                        added=added+1
                                        gatto.append(i)
                                        gatto.append(j-1)
                                        gatto.append(b)
                                        gatto.append(2)
                                        if (best_time<hartis2[i][j-1]):
                                                stay=0
                                                if(mesa1==0 or(i<=best_i and j-1<best_j)or best_time+1 <hartis2[i][j-1]):
                                                            koutaki=b+(posastoixeiamesa//4-a+added-1)*4
                                                            best_time=hartis2[i][j-1]-1
                                                            mesa1=1
                                                            best_i=i
                                                            best_j=j-1
                                        hartis2[i][j-1]=-5
                                if((j+1<stiles)and (hartis2[i][j+1]>time)): #right
                                        added=added+1
                                        gatto.append(i)
                                        gatto.append(j+1)
                                        gatto.append(b)
                                        gatto.append(3)
                                        if (best_time<hartis2[i][j+1]):
                                                stay=0
                                                if(mesa1==0 or best_time+1 <hartis2[i][j+1]):
                                                            koutaki=b+(posastoixeiamesa//4-a+added-1)*4
                                                            best_time=hartis2[i][j+1]-1
                                                            mesa1=1
                                                            best_i=i
                                                            best_j=j+1	
                                        hartis2[i][j+1]=-5
                                if((i-1>=0) and (hartis2[i-1][j]>time)): #up
                                        added=added+1
                                        gatto.append(i-1)
                                        gatto.append(j)
                                        gatto.append(b)
                                        gatto.append(4)
                                        if (best_time<hartis2[i-1][j]):
                                                stay=0
                                                if(mesa1==0 or(i-1<best_i)or best_time+1 <hartis2[i-1][j]):
                                                            koutaki=b+(posastoixeiamesa//4-a+added-1)*4
                                                            best_time=hartis2[i-1][j]-1
                                                            best_i=i-1
                                                            best_j=j
                                        hartis2[i-1][j]=-5
                                b=b+4
                        time=time+1
                        mesa1=0
                        if(posastoixeiamesa==0):
                                break
                                            
        if foreversafe==1:
                print("infinity")
        elif(best_time>-1):
                print(best_time)
        kiniseis=collections.deque()                             
        while(b>-1):
                b=gatto[koutaki+2]
                tempo=gatto[koutaki+3]
                koutaki=b
                if(tempo==1):
                        kiniseis.appendleft('D')
                elif(tempo==2):
                        kiniseis.appendleft('L')
                elif(tempo==3):
                        kiniseis.appendleft('R')
                elif(tempo==4):
                        kiniseis.appendleft('U')
        megethoskin=len(kiniseis) 
        if(stay==1):
                print("stay")
        else:
                #print ("".join(map(str,kiniseis)))
                print(*kiniseis,sep='')

def plimmira(hartis2,aqua,grammes,stiles):
	time=1
	while 1:
		hassthchanged=0
		posastoixeiamesa=0
		b=0
		if(len(aqua)>0):
			hassthchanged=1
			posastoixeiamesa=len(aqua)
			i=aqua[0]
			j=aqua[1]
		for a in range (posastoixeiamesa//2):
			if ((i+1 <grammes) and ((hartis2[i+1][j]==-1)or (hartis2[i+1][j]==-3))):
				aqua.append(i+1)
				aqua.append(j)
				hartis2[i+1][j]=time
			if((i-1>=0) and ((hartis2[i-1][j]==-1) or (hartis2[i-1][j]==-3))):
				aqua.append(i-1)
				aqua.append(j)
				hartis2[i-1][j]=time
			if((j+1<stiles)and((hartis2[i][j+1]==-1)or (hartis2[i][j+1]==-3))):
				aqua.append(i)
				aqua.append(j+1)
				hartis2[i][j+1]=time
			if((j-1>=0) and ((hartis2[i][j-1]==-1)or(hartis2[i][j-1]==-3))):
				aqua.append(i)
				aqua.append(j-1)
				hartis2[i][j-1]=time
			b=b+2
			aaaaa1=aqua.popleft()
			aaaaa2=aqua.popleft()
			if len(aqua)>0:
                                i=aqua[0]
                                j=aqua[1]
		if hassthchanged==0:	
			break
		time=time+1
	return hartis2	
#pleon exei plimmirisei o hartis2

if __name__ == "__main__":
        import sys
        if len(sys.argv) > 1:
                with open(sys.argv[1], "rt") as f:
                        process(f)
        else:
                process(sys.stdin)

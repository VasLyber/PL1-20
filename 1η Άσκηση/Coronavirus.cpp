#include<stdio.h>
#include<iostream>
#include<bits/stdc++.h>
#include<list>
using namespace std;

// ΠΗΓΕΣ
// https://www.geeksforgeeks.org/depth-first-search-or-dfs-for-a-graph/
// https://www.geeksforgeeks.org/detect-cycle-in-a-graph/
// https://www.sanfoundry.com/cpp-program-check-undirected-graph-connected-bfs/

// A C++ Program to detect cycle in a graphvector

class Graph
{
int V;    // No. of vertices
list<int> *adj;    // Pointer to an array containing adjacency lists
bool isCyclicUtil(int v, bool visited[], bool *rs);  // used by isCyclic()
void DFSUtil(int v, bool visited[]);
public:
Graph(int V);   // Constructor
void addEdge(int v, int w);   // to add an edge to graph
void delEdge(int v, int w); // to remove an edge from graph
bool isCyclic();    // returns true if there is a cycle in this graph
void DFS(int v);
void BFS(int s, bool visited[]);
bool isConnected();
Graph getTranspose();
};

Graph::Graph(int V)
{
this->V = V;
adj = new list<int>[V];
}

void Graph::addEdge(int v, int w)
{
adj[v].push_back(w); // Add w to v’s list.
}

void Graph::delEdge(int v, int w)
{

std::list<int>::iterator it1,it2;
// Traversing through the first vector list
// and removing the second element from it
it1 = adj[v].begin();
for (int i : adj[v]){
  if (i == w) {
      adj[v].erase(it1);
      break;
  }
  it1++;
}

// Traversing through the second vector list
// and removing the first element from it

it2 = adj[w].begin();
for (int i : adj[w]){
  if (i == v) {
      adj[w].erase(it2);
      break;
  }
  it2++;
}
}
// This function is a variation of DFSUtil() in https://www.geeksforgeeks.org/archives/18212
bool Graph::isCyclicUtil(int v, bool visited[], bool *recStack)
{
if(visited[v] == false)
{
    // Mark the current node as visited and part of recursion stack
    visited[v] = true;
    recStack[v] = true;

    // Recur for all the vertices adjacent to this vertex
    list<int>::iterator i;
    for(i = adj[v].begin(); i != adj[v].end(); ++i)
    {
        if ( !visited[*i] && isCyclicUtil(*i, visited, recStack) )
            return true;
        else if (recStack[*i])
            return true;
    }

}
recStack[v] = false;  // remove the vertex from recursion stack
return false;
}

// Returns true if the graph contains a cycle, else false.
// This function is a variation of DFS() in https://www.geeksforgeeks.org/archives/18212
bool Graph::isCyclic()
{
// Mark all the vertices as not visited and not part of recursion
// stack
bool *visited = new bool[V];
bool *recStack = new bool[V];
for(int i = 0; i < V; i++)
{
    visited[i] = false;
    recStack[i] = false;
}

// Call the recursive helper function to detect cycle in different
// DFS trees
for(int i = 0; i < V; i++)
    if (isCyclicUtil(i, visited, recStack))
        return i;

return false;
}

void Graph::DFSUtil(int v, bool visited[])
{
// Mark the current node as visited and
// print it
visited[v] = true;
cout << v << " ";

// Recur for all the vertices adjacent
// to this vertex
list<int>::iterator i;
for (i = adj[v].begin(); i != adj[v].end(); ++i)
    if (!visited[*i])
        DFSUtil(*i, visited);
}

// DFS traversal of the vertices reachable from v.
// It uses recursive DFSUtil()
void Graph::DFS(int v)
{
// Mark all the vertices as not visited
bool *visited = new bool[V];
for (int i = 0; i < V; i++)
    visited[i] = false;

// Call the recursive helper function
// to print DFS traversal
DFSUtil(v, visited);
}

void Graph::BFS(int s, bool visited[])
{
list<int> q;
list<int>::iterator i;
visited[s] = true;
q.push_back(s);
while (!q.empty())
{
    s = q.front();
    q.pop_front();
    for(i = adj[s].begin(); i != adj[s].end(); ++i)
    {
        if(!visited[*i])
        {
            visited[*i] = true;
            q.push_back(*i);
        }
    }
}
}

Graph Graph::getTranspose()
{
Graph g(V);
for (int v = 0; v < V; v++)
{
    list<int>::iterator i;
    for(i = adj[v].begin(); i != adj[v].end(); ++i)
    {
        g.adj[*i].push_back(v);
    }
}
return g;
}

bool Graph::isConnected()
{
bool visited[V];
for (int i = 0; i < V; i++)
    visited[i] = false;
BFS(0, visited);
for (int i = 0; i < V; i++)
    if (visited[i] == false)
        return false;
Graph gr = getTranspose();
for(int i = 0; i < V; i++)
    visited[i] = false;
gr.BFS(0, visited);
for (int i = 0; i < V; i++)
    if (visited[i] == false)
        return false;
return true;
}

int main(){

int T,N,M,K,L,temp;
cin >> T;
for (int i=0; i<T; i++){
cin >> N;
cin >> M;
Graph g(N);
for (int j=0; j<M; j++){
  cin >> K >> L;
  g.addEdge(K-1, L-1);
  g.addEdge(L-1, K-1);
}

if(temp = g.isCyclic() && g.isConnected())
    for (int j=0; j<N; j++){
      g.delEdge(i,j);
    }
  printf("hello");
else
    printf("Graph %d NO CORONA ",i);
}
return 0;
}

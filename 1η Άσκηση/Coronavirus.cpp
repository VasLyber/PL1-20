#include<stdio.h>
#include<iostream>
#include<bits/stdc++.h>
#include<list>
using namespace std;

// ΠΗΓΕΣ
// https://www.geeksforgeeks.org/depth-first-search-or-dfs-for-a-graph/
// https://www.geeksforgeeks.org/detect-cycle-undirected-graph/
// https://www.sanfoundry.com/cpp-program-check-undirected-graph-connected-bfs/

// A C++ Program to detect cycle in a graphvector

class Graph
{
int V;    // No. of vertices
list<int> *adj;    // Pointer to an array containing adjacency lists
bool isCyclicUtil(int v, bool visited[], int parent);
void DFSUtil(int v, bool visited[]);
public:
Graph(int V);   // Constructor
void addEdge(int v, int w);   // to add an edge to graph
void delEdge(int v, int w); // to remove an edge from graph
bool isCyclic(int &d);   // returns true if there is a cycle
void DFS(int v);
void BFS(int s, bool visited[]);
bool isConnected();
Graph getTranspose();
void printGraph() ;
};


void Graph:: printGraph()
{
    for (int v = 0; v < V; ++v)
    {
        cout << "\n Adjacency list of vertex "
             << v << "\n head ";
        for (auto x : adj[v])
           cout << "-> " << x;
        printf("\n");
    }
}
Graph::Graph(int V)
{
this->V = V;
adj = new list<int>[V];
}

void Graph::addEdge(int v, int w)
{
adj[v].push_back(w);
}

void Graph::delEdge(int v, int w)
{

std::list<int>::iterator it1,it2;

it1 = adj[v].begin();
for (int i : adj[v]){
  if (i == w) {
      adj[v].erase(it1);
      break;
  }
  it1++;
}

it2 = adj[w].begin();
for (int i : adj[w]){
  if (i == v) {
      adj[w].erase(it2);
      break;
  }
  it2++;
}
}
// A recursive function that uses visited[] and parent to detect
// cycle in subgraph reachable from vertex v.
bool Graph::isCyclicUtil(int v, bool visited[], int parent)
{
    // Mark the current node as visited
    visited[v] = true;

    // Recur for all the vertices adjacent to this vertex
    list<int>::iterator i;
    for (i = adj[v].begin(); i != adj[v].end(); ++i)
    {
        // If an adjacent is not visited, then recur for that adjacent
        if (!visited[*i])
        {
           if (isCyclicUtil(*i, visited, v))
              return true;
        }

        // If an adjacent is visited and not parent of current vertex,
        // then there is a cycle.
        else if (*i != parent)
           return true;
    }
    return false;
}

// Returns true if the graph contains a cycle, else false.
bool Graph::isCyclic(int &d)
{
    // Mark all the vertices as not visited and not part of recursion
    // stack
    bool *visited = new bool[V];
    for (int i = 0; i < V; i++)
        visited[i] = false;

    // Call the recursive helper function to detect cycle in different
    // DFS trees
    for (int u = 0; u < V; u++)
        if (!visited[u]) // Don't recur for u if it is already visited
          if (isCyclicUtil(u, visited, -1)){
            d = u;
            return true;
          }
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
if(g.isCyclic(temp) && g.isConnected()){

  //  g.printGraph();
    for(int j = 0; j < N; ++j){
      g.delEdge(temp,j);
    }
//    g.printGraph();
    if(g.isCyclic(temp))
      printf("Graph %d NO CORONA ",i);
    else{
      printf("Graph %d CORONA ",i);
    }
}
else
    printf("Graph %d NO CORONA ",i);
}
return 0;
}

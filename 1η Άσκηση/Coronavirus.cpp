#include<stdio.h>
#include<iostream>
#include<bits/stdc++.h>
#include<list>
#include<fstream>
#include <algorithm>
using namespace std;

// ΠΗΓΕΣ
// https://www.geeksforgeeks.org/depth-first-search-or-dfs-for-a-graph/
// https://www.geeksforgeeks.org/detect-cycle-undirected-graph/
// https://www.sanfoundry.com/cpp-program-check-undirected-graph-connected-bfs/

bool Contains(const std::list<long long int> &list, long long int x)
{
	return std::find(list.begin(), list.end(), x) != list.end();
}

class Graph
{
long long int V;    // No. of vertices
bool isCyclicUtil(long long int v, bool visited[],long long int parent);
bool isCyclicUtil1(long long int v, bool visited[],long long int parent,list<long long int>& lis);

public:
long long int q =0;
list<long long int> mylist;
list<long long int>::iterator it;
list<long long int> *adj;    // Pointer to an array containing adjacency lists
Graph(long long int V);   // Constructor
void addEdge(long long int v, long long int w);   // to add an edge to graph
void delEdge(long long int v, long long int w); // to remove an edge from graph
bool isCyclic();   // returns true if there is a cycle
bool isCyclic1(list<long long int>& lis);   // returns true if there is a cycle
void BFS(long long int s, bool visited[]);
bool isConnected();
Graph getTranspose();
void printGraph() ;
void connectedComponents();
void DFSUtil(long long int v, bool visited[]);

};
void Graph::connectedComponents()
{
    // Mark all the vertices as not visited
    bool *visited = new bool[V];
    for(long long int v = 0; v < V; v++)
        visited[v] = false;

    for (long long int v=0; v<V; v++)
    {
        if (visited[v] == false)
        {
            DFSUtil(v, visited);
            mylist.push_back(q);
            q=0;
        }
    }
    delete[] visited;
}

void Graph::DFSUtil(long long int v, bool visited[])
{
    // Mark the current node as visited and print it
    visited[v] = true;
    q++;

    // Recur for all the vertices
    // adjacent to this vertex
    list<long long int>::iterator i;
    for(i = adj[v].begin(); i != adj[v].end(); ++i)
        if(!visited[*i])
            DFSUtil(*i, visited);
}

void Graph:: printGraph()
{
  for (long long int v = 0; v < V; ++v)
  {
      cout << "\n Adjacency list of vertex "
           << v << "\n head ";
      for (auto x : adj[v])
         cout << "-> " << x;
      printf("\n");
  }
}

Graph::Graph(long long int V)
{
this->V = V;
adj = new list<long long int>[V];
}

void Graph::addEdge(long long int v, long long int w)
{
adj[v].push_back(w);
}

void Graph::delEdge(long long int v, long long int w)
{

std::list<long long int>::iterator it1,it2;

it1 = adj[v].begin();
for (long long int i : adj[v]){
  if (i == w) {
      adj[v].erase(it1);
      break;
    }
  it1++;
}

it2 = adj[w].begin();
for (long long int i : adj[w]){
  if (i == v) {
      adj[w].erase(it2);
      break;
    }
  it2++;
}
}

bool Graph::isCyclicUtil1(long long int v, bool visited[], long long int parent,list<long long int>& lis)
{
  visited[v] = true;
  list<long long int>::iterator i;
  for (i = adj[v].begin(); i != adj[v].end(); ++i)
  {
      if (!visited[*i])
      {
         if (isCyclicUtil1(*i, visited, v,lis) && !Contains(lis,*i)){
            lis.push_back(*i);
            return true;
          }

      }

      else if (*i != parent && !Contains(lis,*i)){
        lis.push_back(*i);
        return true;
       }
  }
  return false;
}

// Παραλλαγή για να κρατάμε τι έχει επισκεφτεί
bool Graph::isCyclic1(list<long long int>& lis)
{

  bool *visited = new bool[V];
  for (long long int i = 0; i < V; i++)
      visited[i] = false;

  for (long long int u = 0; u < V; u++)
      if (!visited[u])
        if (isCyclicUtil1(u, visited, -1,lis)){
          return true;
        }
  return false;
}

bool Graph::isCyclicUtil(long long int v, bool visited[], long long int parent)
{
  visited[v] = true;

  list<long long int>::iterator i;
  for (i = adj[v].begin(); i != adj[v].end(); ++i)
  {
      if (!visited[*i])
      {
         if (isCyclicUtil(*i, visited, v))
            return true;
      }

      else if (*i != parent)
         return true;
  }
  return false;
}

bool Graph::isCyclic()
{

  bool *visited = new bool[V];
  for (long long int i = 0; i < V; i++)
      visited[i] = false;


  for (long long int u = 0; u < V; u++)
      if (!visited[u])
        if (isCyclicUtil(u, visited, -1)){
          return true;
        }
  return false;
}

void Graph::BFS(long long int s, bool visited[])
{
list<long long int> w;
list<long long int>::iterator i;
visited[s] = true;
w.push_back(s);
  while (!w.empty())
  {
    s = w.front();
    w.pop_front();
    for(i = adj[s].begin(); i != adj[s].end(); ++i)
    {
        if(!visited[*i])
        {
            visited[*i] = true;
            w.push_back(*i);
          }
    }
  }
}

Graph Graph::getTranspose()
{
Graph g(V);
for (long long int v = 0; v < V; v++)
{
  list<long long int>::iterator i;
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
  for (long long int i = 0; i < V; i++)
    visited[i] = false;
  BFS(0, visited);
  for (long long int i = 0; i < V; i++)
    if (visited[i] == false)
      return false;
  Graph gr = getTranspose();
  for(long long int i = 0; i < V; i++)
    visited[i] = false;
  gr.BFS(0, visited);
  for (long long int i = 0; i < V; i++)
    if (visited[i] == false)
      return false;
  return true;
}

int main(int argc, char* argv[]){

ifstream myReadFile;
long long int T,N,M,K,L;
long long int p = 0;
list<long long int> lista;

myReadFile.open(argv[1]);
myReadFile >> T;
for (long long int i=0; i<T; i++){
  myReadFile >> N >> M;
  Graph g(N);
  for (long long int j=0; j<M; j++){
    myReadFile >> K >> L;
    g.addEdge(K-1, L-1);
    g.addEdge(L-1, K-1);
  }
  if(g.isCyclic() && g.isConnected()){

    //g.printGraph();
    p=0;
    lista.clear();
    g.isCyclic1(lista);
    for (auto v : lista){
      for(auto u : lista){
        g.delEdge(u,v);
      }
      p++;
    }

    //g.printGraph();
    if(g.isCyclic()){
      printf("NO CORONA\n");
    }
    else{
        printf("CORONA %llu\n",p);
        g.connectedComponents();
        g.mylist.sort();

          for (auto it = g.mylist.begin(); it != g.mylist.end(); ++it){
            if(it == prev(g.mylist.end())){
              cout << *it << "\n";
              }
            else{
                cout << *it << ' ';
              }

          }
    }
  }
  else{
    printf("NO CORONA\n");
  }
}
myReadFile.close();
return 0;
}

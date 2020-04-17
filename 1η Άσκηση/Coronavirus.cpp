#include<stdio.h>
#include<iostream>
#include<bits/stdc++.h>
#include<list>
#include<fstream>
using namespace std;

// ΠΗΓΕΣ
// https://www.geeksforgeeks.org/depth-first-search-or-dfs-for-a-graph/
// https://www.geeksforgeeks.org/detect-cycle-undirected-graph/
// https://www.sanfoundry.com/cpp-program-check-undirected-graph-connected-bfs/

class Graph
{
int V;    // No. of vertices
bool isCyclicUtil(int v, bool visited[], int parent);
bool isCyclicUtil1(int v, bool visited[], int parent,list<int>& lis);

public:
int q =0;
list<int> mylist;
list<int>::iterator it;
list<int> *adj;    // Pointer to an array containing adjacency lists
Graph(int V);   // Constructor
void addEdge(int v, int w);   // to add an edge to graph
void delEdge(int v, int w); // to remove an edge from graph
bool isCyclic(int &d);   // returns true if there is a cycle
bool isCyclic1(int &d,list<int>& lis);   // returns true if there is a cycle
void BFS(int s, bool visited[]);
bool isConnected();
Graph getTranspose();
void printGraph() ;
void connectedComponents();
void DFSUtil(int v, bool visited[]);

};
void Graph::connectedComponents()
{
    // Mark all the vertices as not visited
    bool *visited = new bool[V];
    for(int v = 0; v < V; v++)
        visited[v] = false;

    for (int v=0; v<V; v++)
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

void Graph::DFSUtil(int v, bool visited[])
{
    // Mark the current node as visited and print it
    visited[v] = true;
    q++;

    // Recur for all the vertices
    // adjacent to this vertex
    list<int>::iterator i;
    for(i = adj[v].begin(); i != adj[v].end(); ++i)
        if(!visited[*i])
            DFSUtil(*i, visited);
}

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
bool Graph::isCyclicUtil1(int v, bool visited[], int parent,list<int>& lis)
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
         if (isCyclicUtil1(*i, visited, v,lis)){
            lis.push_back(*i);
            return true;
          }

      }

      // If an adjacent is visited and not parent of current vertex,
      // then there is a cycle.
      else if (*i != parent){
         lis.push_back(*i);
         return true;
       }
  }
  return false;
}

// Παραλλαγή για να κρατάμε τι έχει επισκεφτεί
bool Graph::isCyclic1(int &d,list<int>& lis)
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
        if (isCyclicUtil1(u, visited, -1,lis)){
          d = u;
          return true;
        }
  return false;
}

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

// QuickSort

void swap(int* a, int* b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

int partition (int arr[], int low, int high)
{
    int pivot = arr[high];    // pivot
    int i = (low - 1);  // Index of smaller element

    for (int j = low; j <= high- 1; j++)
    {
        // If current element is smaller than or
        // equal to pivot
        if (arr[j] <= pivot)
        {
            i++;    // increment index of smaller element
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
}

void quickSort(int arr[], int low, int high)
{
    if (low < high)
    {
        /* pi is partitioning index, arr[p] is now
           at right place */
        int pi = partition(arr, low, high);

        // Separately sort elements before
        // partition and after partition
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

int main(int argc, char* argv[]){

ifstream myReadFile;
myReadFile.open(argv[1]);
int T,N,M,K,L,temp;
int p=0;
list<int> lista;
myReadFile >> T;
for (int i=0; i<T; i++){
  myReadFile >> N >> M;
  Graph g(N);
  for (int j=0; j<M; j++){
    myReadFile >> K >> L;
    g.addEdge(K-1, L-1);
    g.addEdge(L-1, K-1);
  }
  if(g.isCyclic(temp) && g.isConnected()){

    //  g.printGraph();
    g.isCyclic1(temp,lista);
    for (auto v : lista){
      for(auto u : lista){
        g.delEdge(u,v);
      }
      p++;
    }

    //g.printGraph();
    if(g.isCyclic(temp))
      printf("NO CORONA\n");
      else{
        printf("CORONA %d\n",p);
        g.connectedComponents();
        int arr[g.mylist.size()];
        int k = 0;
  	     for (int const &j: g.mylist) {
  		       arr[k++] = j;
  	        }
            quickSort(arr,0,p-1);
            for (int j=0; j < p-1; j++)
              printf("%d ", arr[j]);
            printf("%d\n",arr[p-1]);
          }
        }
        else
        printf("NO CORONA\n");
      }

      myReadFile.close();
      return 0;
}

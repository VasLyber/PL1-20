#include <iostream>
#include<stdio.h>
#include<fstream>
using namespace std;

// function to convert decimal to binary
void decToBinary(int n, int (&binaryNum)[32], int& k, int& l)
{
    // counter for binary array
    int i = 0;
    int p = 0;
    while (n > 0) {

        if((n % 2)==1){
          p++;
        }
        binaryNum[i] = n % 2;
        n = n / 2;
        i++;
    }
    k = p;
    l = i;
}

int main(int argc, char* argv[]){

  ifstream myReadFile;
  int T,N,K,n,a;
  myReadFile.open(argv[1]);
  myReadFile >> T;
  int binarylist[32];

  for (int i=0; i<T; i++){
    myReadFile >> N >> K;
    decToBinary(N,binarylist,n,a);
    if(n<=K){
      while(n<K){
        for(int j=1; j<=a-1; j++){
          if(binarylist[j]>0){
            binarylist[j]--;
            binarylist[j-1] = binarylist[j-1] + 2;
            n++;
            break;
          }
        }
      }
      for(int i=0;i<=a-1;i++)
       cout << binarylist[i];
      cout << "\n";
   }
    else{
      cout << "[]\n";
    }
  }

myReadFile.close();
return 0;
}

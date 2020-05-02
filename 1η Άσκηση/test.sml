val Graph = Array.array(9,[0]);

Array.update(Graph,5,[2]);
Array.update(Graph,2,[5]);
Array.update(Graph,6,[0]);
Array.update(Graph,0,[6]);
Array.update(Graph,7,[8]);
Array.update(Graph,8,[7]);
Array.update(Graph,5,[3]);
Array.update(Graph,3,[5]);
Array.update(Graph,4,[0]);
Array.update(Graph,0,4::Array.sub(Graph,0));
Array.update(Graph,0,8::Array.sub(Graph,0));
Array.update(Graph,8,0::Array.sub(Graph,8));
Array.update(Graph,1,[4]);
Array.update(Graph,4,1::Array.sub(Graph,4));
Array.update(Graph,0,3::Array.sub(Graph,0));
Array.update(Graph,3,0::Array.sub(Graph,3));
Array.update(Graph,4,3::Array.sub(Graph,4));
Array.update(Graph,3,4::Array.sub(Graph,3));
fun sin k = k + 1
fun plin k = k - 1

val Graph1 = Array.array(3,[0]);

Array.update(Graph1,1,[2]);
Array.update(Graph1,2,[1]);
Array.update(Graph1,0,[2]);
Array.update(Graph1,2,0::Array.sub(Graph1,2));


fun isCyclic go =
  let
    val visited = Array.array(3,0);
    val bool = Array.array(1,0);
    fun isCyclicUtil(v,prnt) =
      let
        val neighbour = Array.sub(Graph1,v)
        val numberneighbour = List.length(neighbour)
        fun checkneighbour(n) =
          let
            val neighb = Array.sub(Array.fromList(neighbour),n)
            val vstd = Array.sub(visited,neighb)
          in
            if(vstd=0 andalso (n < numberneighbour)
            then (checkneighbour(sin(n)); isCyclicUtil (n,v))
            else if(n<>prnt andalso (n < numberneighbour)) then Array.update(bool,0,1)
            else()
          end
      in
        Array.update(visited,v,1);
        checkneighbour(0)
      end
  in
    isCyclicUtil(0,~1);
    bool
  end

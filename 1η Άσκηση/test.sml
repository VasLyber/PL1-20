val Graph = Array.array(9,[0]);

Array.update(Graph,5,[2]);
Array.update(Graph,2,[5]);
Array.update(Graph,6,[0]);
Array.update(Graph,0,[6]);
Array.update(Graph,7,[8]);
Array.update(Graph,8,[7]);
Array.update(Graph,5,3::Array.sub(Graph,5));
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
fun arrayToList arr = Array.foldr (op ::) [] arr

val Graph1 = Array.array(3,[0]);

Array.update(Graph1,1,[2]);
Array.update(Graph1,2,[1]);
Array.update(Graph1,0,[2]);
Array.update(Graph1,2,0::Array.sub(Graph1,2));

(*
fun inlist nu rmvlst =
  let
    val leng=List.length(rmvlst)
    fun loop n =
      let
        val g = List.nth(rmvlst,n)
      in
        if(n<=plin(leng) andalso (g=nu)) then true
        else if(n<plin(leng))then(loop(sin(n)))
        else false
      end
  in
    loop(0)
  end
*)
fun ReturnCycle (go) =
  let
    val V = Array.length(go)
    val visited = Array.array(V,0);
    val bool = Array.array(1,0);
    val parentarray = Array.array(V,~1);
    val arxhtelos = Array.array(2,~1);
    fun cycle n =
      if(Array.sub(parentarray,n)<>Array.sub(arxhtelos,1))
        then (n::cycle(Array.sub(parentarray,n)))
      else [n]
    fun isCyclicUtil(v) =
      let
        val neighbour = Array.sub(Graph1,v)
        val numberneighbour = List.length(neighbour)
        fun checkneighbour(n) =
          let
            val neighb = Array.sub(Array.fromList(neighbour),n)
            val vstd = Array.sub(visited,neighb)
          in
            if(vstd=0 andalso (n < plin(numberneighbour)))
              then (   Array.update(parentarray,neighb,v);isCyclicUtil (neighb);
                    checkneighbour(sin(n))
            )
            else if(vstd=0 andalso (n = plin(numberneighbour)))
              then (  Array.update(parentarray,neighb,v);isCyclicUtil (neighb)
                    )
            else if(neighb<>Array.sub(parentarray,v) andalso (n < plin(numberneighbour)))
              then (Array.update(arxhtelos,0,neighb);
                Array.update(arxhtelos,1,v);
                checkneighbour(sin(n));
                Array.update(bool,0,1))
            else if(neighb<>Array.sub(parentarray,v) andalso (n = plin(numberneighbour)))
              then(Array.update(arxhtelos,0,neighb);
                Array.update(arxhtelos,1,v);
                Array.update(bool,0,1))
            else if(n < plin(numberneighbour))
              then (checkneighbour(sin(n)))
            else()
          end
      in
        Array.update(visited,v,1);
        checkneighbour(0)
      end
  in
    isCyclicUtil(0);
    if(Array.sub(bool,0)=1)then Array.sub(arxhtelos,1)::cycle(Array.sub(arxhtelos,0))
    else([])
  end
(*
fun remove(go,rmlist) =
  let
    val lis = List.nth(rmlist,0)
    val gp = arrayToList(Array.sub(go,lis))
    fun rmv nu =
      let
        val gp = arrayToList(Array.sub(go,lis))
      in
        Array.update(gp,)

*)


  fun DFS(go) =
    let
      val V = Array.length(go)
      val visited = Array.array(V,0)
      fun DFSUtil(v) =
        let
          val neighbour = Array.sub(Graph,v)
          val numberneighbour = List.length(neighbour)
          fun checkneighbour(n) =
            let
              val neighb = Array.sub(Array.fromList(neighbour),n)
              val vstd = Array.sub(visited,neighb)
            in
              if(vstd=0 andalso (n < plin(numberneighbour)))
                then (checkneighbour(sin(n)); DFSUtil(neighb))
              else if(vstd=0 andalso (n = plin(numberneighbour)))
                then  DFSUtil(neighb)
              else if(n < plin(numberneighbour))
                then checkneighbour(sin(n))
              else()
            end
        in
          Array.update(visited,v,1)
        end
    in
      DFSUtil(0)
    end

local

  fun parse file =
    let
      fun next_int input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
      val stream = TextIO.openIn file
      val n = next_int stream
      val _ = TextIO.inputLine stream
      fun scanner 0 acc n = (TextIO.closeIn stream; acc)
        | scanner i acc n =
        let
          val d = next_int stream
          val v = next_int stream
        in
          scanner (i - 1) ((d,v) :: acc) n
        end
    in
      rev(scanner n [] n)
    end

  fun sin k = k + 1
  fun plin k = k - 1
  fun arrayToList arr = Array.foldr (op ::) [] arr
  fun printList(xs)= (print("CORONA ");print(String.concatWith " "
    (map Int.toString xs));print("\n"))
    type intlist = int list
    type partition = intlist * int * intlist

  fun quicksort [] = []
    | quicksort (pivot :: xs) =
        partition xs ([], pivot, [])

  and partition [] (lt, pivot, ge) =
      let
        val ltSorted = quicksort lt
        val geSorted = quicksort ge
        val Sorted = ltSorted @ [pivot] @ geSorted
      in
        Sorted
      end
    | partition (x :: xs) (lt, pivot, ge) =
        if x < pivot
        then partition xs (x :: lt, pivot, ge)
        else partition xs (lt, pivot, x:: ge)

  fun inlist(nu,rmvlst) =
    let
      val leng=List.length(rmvlst)
      fun loop n =
        let
          val g = List.nth(rmvlst,n)
        in
          if(n<=plin(leng) andalso g=nu) then true
          else if(n<plin(leng))then(loop(sin(n)))
          else false
        end
    in
      loop(0)
    end
  fun ReturnCycle (go) =
    let
      val V = Array.length(go)
      val visited = Array.array(V,0);
      val bool = Array.array(1,0);
      val parentarray = Array.array(V,~1);
      val arxhtelos = Array.array(2,~1);
      val connected = Array.array(1,0);
      fun cycle n =
        if(Array.sub(parentarray,n)<>Array.sub(arxhtelos,1))
          then (n::cycle(Array.sub(parentarray,n)))
          else [n]
      fun isCyclicUtil(v) =
        let
          val neighbour = Array.sub(go,v)
          val numberneighbour = List.length(neighbour)
          fun checkneighbour(n) =
            let
              val neighb = Array.sub(Array.fromList(neighbour),n)
              val vstd = Array.sub(visited,neighb)
            in
              if(vstd=0 andalso (n < plin(numberneighbour)))
                then (   Array.update(parentarray,neighb,v);isCyclicUtil (neighb);
                checkneighbour(sin(n)))
              else if(vstd=0 andalso (n = plin(numberneighbour)))
                then (  Array.update(parentarray,neighb,v);isCyclicUtil (neighb))
              else if(neighb<>Array.sub(parentarray,v)
              andalso (n < plin(numberneighbour)))
                then (Array.update(arxhtelos,0,neighb);
                Array.update(arxhtelos,1,v);
                checkneighbour(sin(n));
                Array.update(bool,0,sin(Array.sub(bool,0))))
              else if(neighb<>Array.sub(parentarray,v)
              andalso (n = plin(numberneighbour)))
                then(Array.update(arxhtelos,0,neighb);
                Array.update(arxhtelos,1,v);
                Array.update(bool,0,sin(Array.sub(bool,0))))
              else if(n < plin(numberneighbour))
                then (checkneighbour(sin(n)))
              else()
            end
        in
          Array.update(visited,v,1);
          Array.update(connected,0,sin(Array.sub(connected,0)));
          checkneighbour(0)
        end
    in
      isCyclicUtil(0);
      if(plin(Array.sub(bool,0))=1 andalso(Array.sub(connected,0)=V))
        then Array.sub(arxhtelos,1)::cycle(Array.sub(arxhtelos,0))
      else([])
    end

  fun DFS(go,no) =
    let
      val rmvl = ReturnCycle(go)
      val V = Array.length(go)
      val visited = Array.array(V,0)
      val connected = Array.array(1,0);
      fun DFSUtil(v) =
        let
          val neighbour = Array.sub(go,v)
          val numberneighbour = List.length(neighbour)
          fun checkneighbour(n) =
            let
              val neighb = Array.sub(Array.fromList(neighbour),n)
              val vstd = Array.sub(visited,neighb)
            in
              if(vstd=0 andalso (n < plin(numberneighbour)
              andalso not(inlist(neighb,rmvl))))
                then ( DFSUtil(neighb);checkneighbour(sin(n)))
              else if(vstd=0 andalso (n = plin(numberneighbour)
              andalso not(inlist(neighb,rmvl)) ))
                then  (DFSUtil(neighb))
              else if(n < plin(numberneighbour))
                then checkneighbour(sin(n))
              else()
            end
          in
            Array.update(visited,v,1);
            Array.update(connected,0,sin(Array.sub(connected,0)));
            checkneighbour(0)
          end
      in
       DFSUtil(no);
       List.hd(arrayToList(connected))
      end
  fun forall(go,n) =
    if(n<plin(List.length(ReturnCycle(go))))
      then DFS(go,List.nth(ReturnCycle(go),n))::forall(go,sin(n))
    else [DFS(go,List.nth(ReturnCycle(go),n))]
in
  fun coronograph filename =
    let
      val k = parse filename

      val p = ReturnCycle(go)
      val t = List.length(p)
    in
      if(t>O) then printList(quicksort(forall(go,0)))
      else print("NO CORONA")
    end
end

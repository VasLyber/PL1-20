local

  fun arrayToList arr = Array.foldr (op ::) [] arr
  fun parse file =
      let
        	val inStream = TextIO.openIn file

          fun readLines acc =
            let
              val newLineOption = TextIO.inputLine inStream
            in
              if newLineOption = NONE
              then (rev acc)
              else ( readLines ( explode (valOf newLineOption ) :: acc ))
          end;

          val plane = readLines []
          val M = length (hd plane) - 1
          val N = length plane
      in
     	    (N,M,plane)
  end;

  fun tupleCompare ((x1,y1), (x2,y2)) =
    let
      val firstComp = Int.compare (x1,x2)
      val secondComp = Int.compare (y1,y2)
    in
      if (firstComp = EQUAL)
      then secondComp
      else firstComp
    end;

  structure S = BinaryMapFn(
    struct
      type ord_key = int * int
      val compare = tupleCompare
    end
  )

  fun insertList t l row =
    let
      fun insertList_ t [] _ = t
        | insertList_ t [_] _ = t
        | insertList_ t (x::xs) i = insertList_ (S.insert (t,(row,i),x)) xs (i+1)
    in
      insertList_ t l 0
  end;

  fun insert2DList t p =
    let
      fun insert2DList_ t [] _ = t
        | insert2DList_ t (x::xs) i = insert2DList_ (insertList t x i) xs (i+1)
    in
      insert2DList_ t p 0
    end;

  fun findVirusAndTsiord map N M =
    let
      fun loop ~1 ~1 acc = acc
        | loop i j (tsiord,virus,tsiordFin,airportPumps) =
          let
            val cellValue = valOf (S.find (map, (i,j)))

            val newTsiord =
              if cellValue = #"S"
              then (i,j)
              else tsiord

            val newTsiordFin =
              if cellValue = #"T"
              then (i,j)
              else tsiordFin

            val newAirportPumps =
              if cellValue = #"A"
              then ((i,j) :: airportPumps)
              else airportPumps

            val newVirus =
              if cellValue = #"W"
              then (i,j)
              else virus

            val nextIter =
              if j = M-1 andalso i <> N-1
              then (i+1,0)
              else if j = M-1 andalso i = N-1
              then (~1,~1)
              else (i,j+1)
          in
            loop (#1 nextIter) (#2 nextIter) (newTsiord,newVirus,newTsiordFin,newAirportPumps)
          end;
    in
      loop 0 0 ((~1,~1),(~1,~1),(~1,~1),[])
    end;

  fun insertListInFifo([],value,q) = 1
    | insertListInFifo((x::xs),value,q)  =
      let
        val unused = Queue.enqueue(q,(x,value))
      in
        insertListInFifo(xs,value,q)
      end;

  fun insertListinTree [] t _ = t
      | insertListinTree (x::xs) t v = insertListinTree xs (S.insert (t,x,v)) v

  fun sin n = n + 1

  fun plin n = n - 1

  fun exists_in (item: (int*int), mylist: (int*int) list) =
    if null mylist
    then false
    else if #1 (hd mylist) = #1 item andalso #2 (hd mylist) = #2 item
    then true
    else exists_in (item, tl mylist)

  fun findNeighbors3 t (x,y) N M visited =
    let
      val down = (x+1,y)
      val left = (x,y-1)
      val right = (x,y+1)
      val up = (x-1,y)

      val result1 =
      if ( (#1 up) >= 0 ) andalso ((valOf (S.find (t,up))) <> #"X")  andalso (S.find(visited,up) = NONE)
      then [up]
      else []

      val result2 =
      if ( (#2 right) < M ) andalso ((valOf (S.find (t,right))) <> #"X") andalso (S.find(visited,right) = NONE)
      then right::result1
      else result1

      val result3 =
      if ( (#2 left) >= 0 ) andalso ((valOf (S.find (t,left))) <> #"X") andalso (S.find(visited,left) = NONE)
      then left::result2
      else result2

      val result4 =
      if ( (#1 down) < N ) andalso ((valOf (S.find (t,down))) <> #"X") andalso (S.find(visited,down) = NONE)
      then down::result3
      else result3
    in
      result4
  end;
  fun TsiordBFS t N M tsiord home (virusTree: (int * (int * int)) S.map) =
    let
      val q = Queue.mkQueue(): ((int * int) * int) Queue.queue
      val unused = Queue.enqueue(q,(tsiord,0))

      val initialTree = S.insert(S.empty,tsiord,(0,(~1,~1)))

      fun bfsLoop tree true result = (tree,result)
        | bfsLoop tree isQueueEmpty result =
          let
            val NodeAndTime = Queue.dequeue(q)
            val currentNode = (#1 NodeAndTime)
            val time = (#2 NodeAndTime)

            val virusCell = S.find (virusTree,currentNode)
            val virusTime =
            if virusCell = NONE
            then NONE
            else SOME ((#1(valOf virusCell)))

            val notContinuing = virusTime <> NONE andalso (home <> currentNode)andalso(valOf virusTime)  <= time

            val neighbors =
            if notContinuing
            then []
            else findNeighbors3 t currentNode N M tree

            val unused2 =
            if notContinuing
            then 7
            else insertListInFifo(neighbors,(time+1),q)

            val newTree =
            if notContinuing
            then tree
            else insertListinTree neighbors tree (time+1,currentNode)

            val newResult =
            if (notContinuing)
            then ((home,~1))
            else ((home,time))

          in
            if(home = currentNode)then
            (newTree,newResult)
            else bfsLoop newTree (Queue.isEmpty (q)) newResult
      end;
  in
    bfsLoop initialTree (Queue.isEmpty (q)) ((N,M),~2)
  end;

  fun delete (item, list) =
    case list of
    []=>[]
      | xs::ys => if item = xs then delete(item,ys)
          else xs::delete(item,ys)

  fun findMove (x1,y1) (x2,y2) =
    case (x2-x1) of
      1 => "D"
    | ~1  => "U"
    |  0  => case (y2-y1) of
                1 =>"R"
              | ~1 => "L"

  fun getPath node (tsiordTree: (int * (int * int)) S.map) acc =
      let
        val father = #2 (valOf (S.find (tsiordTree, node)))
      in
        if father = (~1,~1)
        then(
          acc
          )
        else(
          getPath father tsiordTree ( (valOf (Char.fromString(findMove father node))) :: acc )
        )
  end;
 fun bfs (t,N,M,startingPoint,airportPos:(int*int) list) =
    let
      val q = Queue.mkQueue(): ((int * int) * int) Queue.queue
      val unused = insertListInFifo(startingPoint,0,q)
      val resultTree = insertListinTree startingPoint S.empty (0,(~1,~1))
      val q2 = Queue.mkQueue(): ((int * int) * int) Queue.queue
      val flag = Array.array(1,0)
      val flag2 = Array.array(1,0)
      val air = Array.array(1,0)
      val times = Array.array(1,0)
      val airport = Array.array(2,0)
      val airportime = Array.array(1,0)

      fun findNeighbors t (x,y) tim N M visited =
        let
          val down = (x+1,y)
          val left = (x,y-1)
          val right = (x,y+1)
          val up = (x-1,y)
          val airport1 = Array.sub(airport,0)
          val airport2 = Array.sub(airport,1)
          val airvalue = (airport1,airport2)
          val newairport = delete(airvalue,airportPos)

          val airp = if (( Array.sub(flag,0)=1 ))
          then  Array.update(air,0,sin(Array.sub(air,0)))
          else ();

          val airp = if (( Array.sub(air,0)=6 ))
          then (
                insertListInFifo(newairport,(Array.sub(airportime,0)+2),q2);
                Array.update(flag,0,0);
                Array.update(flag2,0,1);
                Array.update(air,0,0)
          )
          else ();

          val result1 =
          if ((( (#1 up) >= 0 ) andalso ((valOf (S.find (t,up))) <> #"X")  andalso (S.find(visited,up) = NONE)))
          then [up]
          else []

          val unused4 = if (( (#1 up) >= 0 ) andalso ((valOf (S.find (t,up))) <> #"X")  andalso (S.find(visited,up) = NONE) andalso ( Array.sub(flag,0)=0 ) andalso ( Array.sub(flag2,0)=0 ) andalso  exists_in(up,airportPos))
          then (
            Array.update(airport,0,(#1 up));
            Array.update(airport,1,(#2 up));
            Array.update(airportime,0,tim+5);
            Array.update(flag,0,1)
            )
          else ();

          val result2 =
          if (( (#2 right) < M ) andalso ((valOf (S.find (t,right))) <> #"X") andalso (S.find(visited,right) = NONE))
          then right::result1
          else result1

          val unused4 = if (((#2 right) < M  ) andalso ((valOf (S.find (t,right))) <> #"X")  andalso (S.find(visited,right) = NONE) andalso ( Array.sub(flag,0)=0 )  andalso ( Array.sub(flag2,0)=0 ) andalso  exists_in(right,airportPos))
          then (
            Array.update(airport,0,(#1 right));
            Array.update(airport,1,(#2 right));
            Array.update(airportime,0,tim+5);
            Array.update(flag,0,1)
          )
          else ();

          val result3 =
          if (( (#2 left) >= 0 ) andalso ((valOf (S.find (t,left))) <> #"X") andalso (S.find(visited,left) = NONE))
          then left::result2
          else result2

          val unused4 = if (( (#2 left) >= 0 ) andalso ((valOf (S.find (t,left))) <> #"X") andalso (S.find(visited,left) = NONE) andalso ( Array.sub(flag,0)=0 )  andalso ( Array.sub(flag2,0)=0 ) andalso exists_in(left,airportPos))
          then(
            Array.update(airport,0,(#1 left));
            Array.update(airport,1,(#2 left));
            Array.update(airportime,0,tim+5);
            Array.update(flag,0,1)
            )
          else ();

          val result4 =
          if (( (#1 down) < N ) andalso ((valOf (S.find (t,down))) <> #"X") andalso (S.find(visited,down) = NONE))
          then down::result3
          else result3

          val unused4 = if (( (#1 down) < N ) andalso ((valOf (S.find (t,down))) <> #"X") andalso (S.find(visited,down) = NONE) andalso ( Array.sub(flag,0)=0 ) andalso ( Array.sub(flag2,0)=0 ) andalso   exists_in(down,airportPos))
          then (
            Array.update(airport,0,(#1 down));
            Array.update(airport,1,(#2 down));
            Array.update(airportime,0,tim+5);
            Array.update(flag,0,1)
            )
         else ();
        in
          result4
        end;
      fun findNeighbors2 t (x,y) N M visited =
        let
          val down = (x+1,y)
          val left = (x,y-1)
          val right = (x,y+1)
          val up = (x-1,y)

          val result1 =
          if ( (#1 up) >= 0 ) andalso ((valOf (S.find (t,up))) <> #"X")  andalso (S.find(visited,up) = NONE)
          then [up]
          else []

          val result2 =
          if ( (#2 right) < M ) andalso ((valOf (S.find (t,right))) <> #"X") andalso (S.find(visited,right) = NONE)
          then right::result1
          else result1

          val result3 =
          if ( (#2 left) >= 0 ) andalso ((valOf (S.find (t,left))) <> #"X") andalso (S.find(visited,left) = NONE)
          then left::result2
          else result2

          val result4 =
          if ( (#1 down) < N ) andalso ((valOf (S.find (t,down))) <> #"X") andalso (S.find(visited,down) = NONE)
          then down::result3
          else result3
        in
          result4
      end;

      fun bfsLoop tree true = tree
            | bfsLoop tree isQueueEmpty =
        let
            val NodeAndTime =
            if (not(Queue.isEmpty(q)))then
              (Queue.head(q))
            else ((0,0),0)
            val currentNode = (#1 NodeAndTime);
            val currenTime = (#2 NodeAndTime);
            val neighbors = findNeighbors t currentNode currenTime N M tree;
            val NodeAndTime2 =
            if (not(Queue.isEmpty(q2)))then
            (Queue.head(q2))
            else ((0,0),0)
            val currentNode2 = (#1 NodeAndTime2);
            val neighbors2 = findNeighbors2 t currentNode2 N M tree;
            val airport1 = Array.sub(airport,0)
            val airport2 = Array.sub(airport,1)
            val airvalue = (airport1,airport2)
            val newairport = delete(airvalue,airportPos)

            val newResultTree =
              if ((Array.sub(times,0) mod 2) = 0 )then
              ( if (not(Queue.isEmpty(q))) then(
                  Queue.dequeue(q);
                  insertListInFifo(neighbors,((#2 NodeAndTime)+2),q);
                  insertListinTree neighbors tree (((#2 NodeAndTime)+2),currentNode)
                )
                else(
                  tree
                  )
              )
              else (
                if (not(Queue.isEmpty(q2))) then(
                  if(Array.sub(flag2,0)=1)then
                  (
                      Array.update(flag2,0,0);
                    insertListinTree newairport tree (Array.sub(airportime,0)+2,(Array.sub(airport,0),Array.sub(airport,1)))
                    )
                  else(
                  Queue.dequeue(q2);
                  insertListInFifo(neighbors2,((#2 NodeAndTime2)+2),q2) ;
                  insertListinTree neighbors2 tree (((#2 NodeAndTime2)+2),currentNode2)
                  )
                )
                else(
                tree
                )
              )
        in
              Array.update(times,0,sin(Array.sub(times,0)));
              bfsLoop newResultTree ((Queue.isEmpty(q) andalso Queue.isEmpty(q2)) andalso Array.sub(flag,0)=0)
        end;

  in
    bfsLoop resultTree ((Queue.isEmpty(q) andalso Queue.isEmpty(q2)) andalso Array.sub(flag,0)=0)
  end;
in
  fun stayhome file =
    let
      val out = parse file;
      val tree = insert2DList (S.empty) (#3 out);
      val N = #1 out;
      val M = #2 out;

      val points =  findVirusAndTsiord tree N M;
      val tsiordPos = #1 points;
      val virusPos = #2 points;
      val tsiordFinPos = #3 points;
      val airportPos = #4 points;
      val virusTree = bfs(tree,N,M,[virusPos],airportPos);

      val (tsiordTree,result) = TsiordBFS tree N M tsiordPos tsiordFinPos virusTree;

      val node = #1 result
      val time = #2 result

      val path = if(time>0)then getPath node tsiordTree []
        else []

      val pathInString =
        if path = []
        then "error"
        else implode path

      val timeInString =
        if (time)<0
        then ""
        else Int.toString(time)
      val ok = S.listItems(virusTree)
    in
      if(time>0)then(
        print (timeInString ^ "\n" ^ pathInString ^ "\n")
      )
      else print("IMPOSSIBLE \n")
      
  end;
end;

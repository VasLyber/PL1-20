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

fun insertListInFifo [] value q = 1
  | insertListInFifo (x::xs) value q  =
    let
      val unused = Queue.enqueue(q,(x,value))
    in
      insertListInFifo xs value q
    end;

fun insertListinTree [] t _ = t
    | insertListinTree (x::xs) t v = insertListinTree xs (S.insert (t,x,v)) v

fun sin n = n + 1


fun bfs t N M startingPoint =
  let
    val q = Queue.mkQueue(): ((int * int) * int) Queue.queue
    val unused = insertListInFifo startingPoint 0 q
    val resultTree = insertListinTree startingPoint S.empty (0,(~1,~1))
    val flag = Array.array(1,0)
    val flag2 = Array.array(1,0)
    val air = Array.array(1,0)
    fun findNeighbors t (x,y) N M visited =
      let
        val down = (x+1,y)
        val left = (x,y-1)
        val right = (x,y+1)
        val up = (x-1,y)

        val result1 =
        if ( (#1 up) >= 0 ) andalso ((valOf (S.find (t,up))) <> #"X")  andalso (S.find(visited,up) = NONE)
        then  (if ( Array.sub(flag,0)==1 ) then Array.update(air,0,sin(Array.sub(air,0)))),
              (if ( Array.sub(air,0)==6 ) then
              [up]
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
          val NodeAndTime = Queue.dequeue(q)
          val currentNode = (#1 NodeAndTime)
          val time = (#2 NodeAndTime)
          if ((time mod 2) == 0 )then val neighbors = findNeighbors t currentNode N M tree
          else if ((time mod 2) == 1) val neighbors = findNeighbors2 t currentNode N M tree
          val unused2 = insertListInFifo neighbors (time+1) q
          val newResultTree = insertListinTree neighbors tree (time+1,currentNode)

        in
          bfsLoop newResultTree  (Queue.isEmpty (q))
    end;

  in
    bfsLoop resultTree  (Queue.isEmpty (q))
  end;

fun stayhome file =
  let
    val out = parse file;
    val tree = insert2DList (S.empty) (#3 out)
    val N = #1 out;
    val M = #2 out;

    val points =  findVirusAndTsiord tree N M;
    val tsiordPos = #1 points;
    val virusPos = #2 points;
    val tsiordFinPos = #3 points;
    val airportPos = #4 points;

    val virusTree = bfs tree N M [virusPos];

  in
    virusTree
  end;

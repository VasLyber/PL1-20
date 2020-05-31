fun parse file =
    let
  	    (* Open input file. *)
      	val inStream = TextIO.openIn file

        (* Reads lines until EOF and puts them in a list as char lists *)
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
      | loop i j (tsiord,virus,airportPumps) =
        let
          val cellValue = valOf (S.find (map, (i,j)))

          val newTsiord =
            if cellValue = #"S"
            then (i,j)
            else tsiord

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
          loop (#1 nextIter) (#2 nextIter) (newTsiord,newVirus,newAirportPumps)
        end;
  in
    loop 0 0 (~1,~1,~1)
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
    val airpoPos = #3 points;

  in
    virusPos
  end;

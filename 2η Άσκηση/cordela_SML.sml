local
fun parse file =
    let
	(* A function to read an integer from specified input. *)
        fun readInt input = 
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

	(* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of countries) and consume newline. *)
	val n = readInt inStream
	val t = readInt inStream
	val _ = TextIO.inputLine inStream

        (* A function to read N integers from the open file. *)
	fun readInts 0 acc = acc (* Replace with 'rev acc' for proper order. *)
	  | readInts i acc = readInts (i - 1) (readInt inStream :: acc)
    in
   	(n, t , readInts n [])
    end


structure Key=
 struct
  type ord_key=int
  val compare=Int.compare
 end


fun min x y = if x>y then y else x;


structure mymap = BinaryMapFn ( Key  );

fun increase(list1,list2,front,tail,k,ourmap,minimum,n,t,colorseen)=
 let 
  val number= hd list1
  val elem=mymap.find(ourmap,number)
  val per=getOpt(elem,0)+1
 in
  if (t>k) then
   if (elem<>NONE) then
    if (tl list1 <> []) then increase (tl list1,list2,front+1,tail,k,mymap.insert(ourmap,number,per),minimum,n,t,colorseen)
    else (list1,list2,front,tail,k,ourmap,minimum,colorseen)
   else 
    if (colorseen=t) then 
     if (tl list1 <> []) then increase(tl list1,list2,front+1,tail,k+1,mymap.insert(ourmap,number,per),minimum,n,t,colorseen)
     else (list1,list2,front,tail,k,ourmap,minimum,colorseen)
    else 
     if (tl list1<> []) then increase(tl list1,list2,front+1,tail,k+1,mymap.insert(ourmap,number,per),minimum,n,t,colorseen+1)
     else (list1,list2,front+1,tail,k+1,ourmap,minimum,colorseen+1)
  else (list1,list2,front,tail,k,ourmap,minimum,colorseen)
 end
  
fun decrease(list1,list2,front,tail,k,ourmap,minimum,n,t,colorseen)=
  let 
   val number=hd list2
   val elem=mymap.find(ourmap,number)
   val per=getOpt(elem,0)-1
   val per1=getOpt(elem,0)
   val minimum = min minimum (front-tail+1)
  in
   if (k>=t) then
    if (per1=1) then 
     if (tl list2 <> []) then decrease(list1,tl list2,front,tail+1,k-1,mymap.insert(ourmap,number,per),minimum,n,t,colorseen)
     else (list1,list2,front,tail,k,ourmap,minimum,colorseen)
    else 
     if (tl list2 <> []) then decrease(list1,tl list2,front,tail+1,k,mymap.insert(ourmap,number,per),minimum,n,t,colorseen)
     else (list1,list2,front,tail,k,ourmap,minimum,colorseen)
   else (list1,list2,front,tail,k,ourmap,minimum,colorseen)
  end


fun loop (n,t,list1,list2,front,tail,k,ourmap,minimum,colorseen)=
 let  
  val (list1,list2,front,tail,k,ourmap,minimum,colorseen)=increase(list1,list2,front,tail,k,ourmap,minimum,n,t,colorseen)
 in
  let
    val (list1,list2,front,tail,k,ourmap,minimum,colorseen)= decrease(list1,list2,front,tail,k,ourmap,minimum,n,t,colorseen)
  in 
   if (tl list1 <> [] andalso tl list2<> []) then loop(n,t,list1,list2,front,tail,k,ourmap,minimum,colorseen)
   else 
   	if (colorseen<t) then 0
   	else minimum
  end
 end

fun solve(n,t,acc)=loop(n,t,acc,acc,0,0,0,mymap.empty,n+2,0)
in
fun colors fileName = 
 let
  val r=solve (parse fileName)
 in
  print (Int.toString r);
  print ("\n")
 end
end


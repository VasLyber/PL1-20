fun digits n =
  let 
    fun digits_aux m acc = 
      if m < 10 then m :: acc else digits_aux (m div 10) ((m mod 10) :: acc);
  in
    digits_aux n []
  end;

fun d n = n + List.foldl op+ 0 (digits n);

fun dseq n lim =
  if n > lim then [] else n :: dseq (d n) lim;

fun self i j =
  let
    val bit_array = BitArray.array (j+1,true);

    fun from_i arr i lim =
      if i > lim then arr
      else
	if BitArray.sub (arr,i) = false then from_i arr (i+1) lim
	else
	  let
	    val nums_with_generators = dseq (d i) lim;
	  in
	    (
	     map (fn j => BitArray.update (arr,j,false)) nums_with_generators;
	     from_i arr (i+1) lim
	    )
	  end;

    fun count arr i j =
      if i > j then 0
      else
	let
	  val cnt = count arr (i+1) j;
	in
	  if BitArray.sub (arr,i) then 1 + cnt else cnt
	end;
  in
    count (from_i bit_array 1 j) i j
  end;

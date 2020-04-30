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

	fun plin lk = lk - 1

	fun sin n = n + 1

	fun dec2bin (x) =
		if x <= 1
			then [x]
		else
			dec2bin (x div 2) @ [(x mod 2)];

	fun printList xs = (print("[");
	print(String.concatWith "," (map Int.toString xs));
	print("]");
	print("\n"))
	fun arrayToList arr = Array.foldr (op ::) [] arr
	fun sin2 n = n + 2
in
	fun powers2 filename =
		let
	 		val powers = parse filename
			val len = List.length powers
			fun dec2binall nu leng =
				let
					fun dc2b (n,len) =
							 let
									val x = plin n
									val p = List.nth(powers,x)
									val q = #1 p
									val i = #2 p
							 in
									if n >= len then [dec2bin(q)] else (
									dec2bin(q) :: dc2b ((sin n),len)
									)
								end
				in
							dc2b (nu,leng)
				end
		fun take2all nu leng =
			let
				fun tk2 (n,len) =
							let
								val x = plin n
								val p = List.nth(powers,x)
								val i = #2 p
						 in
								if n >= len then [i] else (
								i :: tk2 ((sin n),len)
								)
							end
					in
								tk2 (nu,leng)
					end

   	fun final num  =
			let
				val AllBin = dec2binall 1 len
				val AllNum = take2all 1 len
				fun count e i j =
				   if i > j then 0
				   else
					  let
					   val cnt = count e (i+1) j;
					  in
					   if (List.nth(e,i)=1) then 1 + cnt else cnt
					  end;
				fun fin  n  =
					let
						 val x = plin n
						 val g = List.rev(List.nth(AllBin, plin n))
						 val go = Array.fromList(g)
						 val i = List.nth(AllNum,x)
						 val length2 = List.length g
						 val length22 = plin length2
						 val ass = count g 0 length22
						 val assoi = Array.array(1,ass)
						 fun drop00 (foo,num) =
						 	List.rev(List.drop(foo,num))
						 fun indexfinder2 (foo,num5) =
							  if(Array.sub(foo,num5)>0)then num5
							  else indexfinder2(foo,sin(num5))
						 fun condition num2 =
						 		let
									fun indexfinder num5 =
											if(Array.sub(go,num5)>0)then num5
											else indexfinder(sin(num5))

									fun updatelist index  =(
										  Array.update(go,index,plin(Array.sub(go,index)));
											Array.update(go,plin(index),sin2(Array.sub(go,plin(index))));
											Array.update(assoi,0,sin(Array.sub(assoi,0)));
											drop00(List.rev(arrayToList(go)),indexfinder2(Array.fromList(List.rev(arrayToList(go))),0)))
								in
						 			if(Array.sub(assoi,0)<i) then (updatelist(indexfinder(1)); condition(num2))
									else if(Array.sub(assoi,0)=i)then (	drop00(List.rev(arrayToList(go)),indexfinder2(Array.fromList(List.rev(arrayToList(go))),0)))
									else []
								end
					  fun ifelse num1=
		 			  	if (num1 <=1) then printList(condition(num1)) else (fin(plin(num1));printList(condition(num1)))
				in
					ifelse n
				end
			in
			 fin len
 		  end;
	 in
		  final len
	 end;

end;

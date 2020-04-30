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

	fun printList xs = print(String.concatWith ", " (map Int.toString xs));

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

   	fun final num len1  =
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
				fun fin n len2 =
					let
						 val x = plin n
						 val g = List.nth(AllBin,x)
						 val go = Array.fromList(g)
						 val i = List.nth(AllNum,x)
						 val length2 = List.length g
						 val length22 = plin length2
						 val ass = count g 0 length22
						 val assoi = Array.array(1,ass)
						 fun condition num2 =
						 		let
									val assoi1 = Array.sub(assoi,0)
									fun indexfinder num5 =
										let
											val tr = Array.sub(go,num5)
										in
											if(tr>0 andalso num5>0 )then num5
											else indexfinder(sin(num5))
										end
									fun updatelist num3  =
										let
											val we = indexfinder 0
											val we1 = plin we
											val numar = Array.sub(go,we)
											val numarr = Array.sub(go,we1)
											val numar1 = plin numar
											val numarr1 = sin2 numarr
											val assoi2 = sin assoi1
										in
										  Array.update(go,we,numar1);
											Array.update(go,we1,numarr1);
											Array.update(assoi,0,assoi2);
											go
										end
									val kop = []
									val kop1 = Array.fromList(kop)
								in
						 			if(assoi1<i) then (updatelist num2; condition num2)
									else if(ass=i)then go
									else kop1
								end
					 fun ifelse num1=
		 				if (num1 >=len2) then condition(num1) else (fin (sin num1); condition (num1))
					in
						ifelse n
					end
			in
			 fin 1 len1
 		  end;
	 in
		  final 1 len
	 end;

end;

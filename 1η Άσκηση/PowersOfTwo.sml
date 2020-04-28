local
	fun parse file =
		let
				fun next_int input =
				Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
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
							 in
									if n >= len then [dec2bin(q)] else (
									dec2bin(q) :: dc2b ((sin n),len)
									)
								end
				in
							dc2b (nu,leng)
				end
   	fun finalresult le =
			let
				val AllBin = dec2binall 1 le
				val p = List.nth(powers,0)
				val q = #2 p
				val z = List.nth(AllBin,2)
				val g = List.length z
				val gi = plin g
				fun count e i j =
				   if i > j then 0
				   else
					  let
					   val cnt = count e (i+1) j;
					  in
					   if (List.nth(e,i)=1) then 1 + cnt else cnt
					  end;
				(*fun mexritelos lista assoi q =
					if(assoi>q) then [] else (
									)
				*)						
			in
			 count z 0 gi
 		  end

	in
		  	finalresult len
	end
end

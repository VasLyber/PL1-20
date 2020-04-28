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
			fun t lk = lk -1
	 		val powers = parse filename
			val len = List.length powers
			val AllBin = []
			fun dec2binall nu leng =
				let
					fun dseq (n,len) =
							 let
									val x = plin n
									val p = List.nth(powers,x)
									val q = #1 p
							 in
									if n >= len then [dec2bin(q)] else (
									dec2bin(q) :: dseq ((sin n),len)@AllBin
									)
								end
				in
							dseq (nu,leng)
				end
		in
		  	dec2binall 1 len
		end
end

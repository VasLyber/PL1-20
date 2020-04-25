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
								scanner (i - 1) ((d, v) :: acc) n
						end
		in
				rev(scanner n [] n)
		end


fun dec2bin (x) =
		if x <= 1
			then [x]
		else
			dec2bin (x div 2) @ [(x mod 2)];

(*in
  fun powers2 filename =
		 val powers = parse filename

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

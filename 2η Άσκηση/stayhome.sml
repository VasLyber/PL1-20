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

fun stayhome file =
  let
    val out = parse file;
  in
    print("hello")
  end;

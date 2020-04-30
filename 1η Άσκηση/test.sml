
  fun sin l = l + 1
  fun sin2 lu = lu +2
  fun plin k = k -1

  fun indexfinder (num5,go) =
    let
      val tr = Array.sub(go,num5)
    in
      if(tr>0 andalso num5>0 )then num5
      else indexfinder(sin(num5),go)
    end


fun listoarr x =
  Array.fromList(x)

fun dec2bin (x) =
		if x <= 1
			then [x]
		else
		  dec2bin (x div 2) @ [(x mod 2)];


fun updatelist go =
  let
    val we = indexfinder (0,go)
    val we1 = plin we
    val numar = Array.sub(go,we)
    val numarr = Array.sub(go,we1)
    val numar1 = plin numar
    val numarr1 = sin2 numarr
  in
    Array.update(go,we,numar1);
    Array.update(go,we1,numarr1);
    go
  end

fun dec2bin (x) =
	if x <= 1
		then [x]
	else
		dec2bin (x div 2) @ [(x mod 2)];

    

(*code taken from https://gist.github.com/nickie/e843bc3cda3e2a37e3a90cc24a5c1dfe?fbclid=IwAR1SqpQwjJMGym_e8qcNJv606t_QUmWY8q6s2rbYMy1NscdriOoMA0XKCvk *)

structure TwoDMap = BinaryMapFn (struct
 type ord_key= int * int
 fun compare ((x1,y1),(x2,y2))=
  if ( x1<x2 orelse ( x1=x2 andalso y1<y2 ) )then LESS else if ( x2<x1 orelse ( x1=x2 andalso y2<y1 ) ) then GREATER else EQUAL
 end);
 
 
fun plim fileName =
let 
	val instream= TextIO.openIn fileName;
	fun toStr c = if c= #"\n" then "\n" else (Char.toString c);
	fun toChar s = map toStr (String.explode (Option.valOf s))
	fun parseIns l x =
		if x=NONE then l
		else parseIns (x :: l) (TextIO.inputLine instream);
	val l= List.map toChar (parseIns [] (TextIO.inputLine instream));
	val n= length l;
	val m = (length (hd l) - 1);
 	val pinwx=Queue.mkQueue()
	val pinwy=Queue.mkQueue()
	val pinax=Queue.mkQueue()
	val pinay=Queue.mkQueue()
	fun loop (l,xartis,X) =
		let 
			val head = hd l 
			fun loop1 (head,xartis,Y)=
					if(head <> [] andalso hd head <> "\n") then( if( hd head="W") then (Queue.enqueue(pinwx,X); Queue.enqueue(pinwy,Y)) else if (hd head ="A") then (Queue.enqueue(pinax,X);Queue.enqueue(pinay,Y)) else(); loop1(tl head,TwoDMap.insert(xartis,(X,Y),(hd head)),Y+1) )
					else (xartis)
			val xartisnew = loop1(head,xartis,0)
		in
			if( tl l <> []) then ( loop(tl l,xartisnew,X+1))
			else (xartisnew)
		end
		
	val xart=loop(List.rev l,TwoDMap.empty,0)
	
	fun is_obst(x,y)= 
		let 
			val obs=TwoDMap.find(xart,(x,y))
			val per=getOpt(obs,"T")
		in
			if (per="X") then 1
			else 0
		end
	
	val qx=Queue.mkQueue()
	val qy=Queue.mkQueue()
	val qt=Queue.mkQueue()
		
	fun flood_fill(n,m,qx,qy,qt,visited,visitedt)=
		if (Queue.isEmpty qx = false) then
			let 
				val x= Queue.head qx
				val y= Queue.head qy
				val t= Queue.head qt
				val vis = TwoDMap.find(visited,(x,y))
				val per = getOpt(vis,2)
				val vist= TwoDMap.find(visitedt,(x,y))
				val pert= getOpt(vist,100000000)
				val visup=TwoDMap.find(visited,(x-1,y))
				val perup=getOpt(visup,2)
				val visright=TwoDMap.find(visited,(x,y+1))
				val perright=getOpt(visright,2)
				val visleft=TwoDMap.find(visited,(x,y-1))
				val perleft=getOpt(visleft,2)
				val visdown=TwoDMap.find(visited,(x+1,y))
				val perdown=getOpt(visdown,2)
			in
				Queue.dequeue qx;
				Queue.dequeue qy;
				Queue.dequeue qt;
				if (per=2 andalso is_obst(x,y)=0) then (
					if (x-1<=n-1 andalso x-1>=0 andalso y>=0 andalso y<=m-1 andalso perup=2) then 
						(Queue.enqueue(qx,x-1); Queue.enqueue(qy,y); Queue.enqueue(qt,t+1))
					else();
					if (x<=n-1 andalso x>=0 andalso y+1>=0 andalso y+1<=m-1 andalso perright=2) then 
						(Queue.enqueue(qx,x); Queue.enqueue(qy,y+1); Queue.enqueue(qt,t+1))
					else();
					if (x<=n-1 andalso x>=0 andalso y-1>=0 andalso y-1<=m-1 andalso perleft=2) then 
						(Queue.enqueue(qx,x); Queue.enqueue(qy,y-1); Queue.enqueue(qt,t+1))
					else();
					if (x+1<=n-1 andalso x+1>=0 andalso y>=0 andalso y<=m-1 andalso perdown=2) then 
						(Queue.enqueue(qx,x+1); Queue.enqueue(qy,y); Queue.enqueue(qt,t+1))
					else();
		
					if (pert>t) then flood_fill(n,m,qx,qy,qt,TwoDMap.insert(visited,(x,y),1),TwoDMap.insert(visitedt,(x,y),t))
					else flood_fill(n,m,qx,qy,qt,TwoDMap.insert(visited,(x,y),1),TwoDMap.insert(visitedt,(x,y),pert))
				)
			 	else flood_fill(n,m,qx,qy,qt,visited,visitedt)
			end
		else (visitedt)

	fun prepforfloodfill(qx,qy,qt,pinwx,pinwy)= 
		if (Queue.isEmpty pinwx = false ) then (
			let
				val hx = Queue.head pinwx
				val hy = Queue.head pinwy
			in
				Queue.enqueue(qx,hx);
				Queue.enqueue(qy,hy);
				Queue.enqueue(qt,0);
				Queue.dequeue pinwx;
				Queue.dequeue pinwy;
				prepforfloodfill(qx,qy,qt,pinwx,pinwy)
			end
		)
		else(qx,qy,qt,pinwx,pinwy)
		
in
	let
		val (qx,qy,qt,pinwx,pinwy) = prepforfloodfill(qx,qy,qt,pinwx,pinwy)
		val timesforbfs = flood_fill(n,m,qx,qy,qt,TwoDMap.empty,TwoDMap.empty)
		val fx=Queue.mkQueue()
		val fy=Queue.mkQueue()
		val ft=Queue.mkQueue()
		val cldby = Queue.mkQueue()
		fun bfs(n,m,fx,fy,ft,cldby,visit,timesforbfs,max,maxx,maxy,calledby)=
			if (Queue.isEmpty fx = false) then
				let
					val fposx= Queue.head fx
					val fposy= Queue.head fy
					val tempt= Queue.head ft
					val caller = Queue.head cldby
					
					val currup= TwoDMap.find(timesforbfs,(fposx-1,fposy))
					val cperup= getOpt(currup,~1)
					val currright= TwoDMap.find(timesforbfs,(fposx,fposy+1))
					val cperright= getOpt(currright,~1)
					val currleft= TwoDMap.find(timesforbfs,(fposx,fposy-1))
					val cperleft= getOpt(currleft,~1)
					val currdown= TwoDMap.find(timesforbfs,(fposx+1,fposy))
					val cperdown= getOpt(currdown,~1)
					
					
					val visitf=TwoDMap.find(visit,(fposx,fposy))
					val vper=getOpt(visitf,~1)
					val visitup=TwoDMap.find(visit,(fposx-1,fposy))
					val vperup=getOpt(visitup,~1)
					val visitright=TwoDMap.find(visit,(fposx,fposy+1))
					val vperright=getOpt(visitright,~1)
					val visitleft=TwoDMap.find(visit,(fposx,fposy-1))
					val vperleft=getOpt(visitleft,~1)
					val visitdown=TwoDMap.find(visit,(fposx+1,fposy))
					val vperdown=getOpt(visitdown,~1)
					val tempsec=TwoDMap.find(timesforbfs,(fposx,fposy))
					val pertempsec=getOpt(tempsec,10000000)-1
				in
					Queue.dequeue fx;
					Queue.dequeue fy;
					Queue.dequeue ft;
					Queue.dequeue cldby;
					if (vper= ~1 andalso is_obst(fposx,fposy)=0) then (
						
						if (fposx+1<=n-1 andalso fposx+1>=0 andalso fposy>=0 andalso fposy<=m-1 andalso (cperdown>tempt+1 orelse cperdown= ~1) andalso vperdown= ~1) then ( Queue.enqueue(fx,fposx+1); Queue.enqueue(fy,fposy); Queue.enqueue(ft,tempt+1);Queue.enqueue(cldby,(fposx,fposy))  )
						else();
						if (fposx<=n-1 andalso fposx>=0 andalso fposy-1>=0 andalso fposy-1<=m-1 andalso (cperleft>tempt+1 orelse cperleft= ~1) andalso vperleft= ~1) then ( Queue.enqueue(fx,fposx); Queue.enqueue(fy,fposy-1); Queue.enqueue(ft,tempt+1);Queue.enqueue(cldby,(fposx,fposy)) )
						else();
						if (fposx<=n-1 andalso fposx>=0 andalso fposy+1>=0 andalso fposy+1<=m-1 andalso (cperright>tempt+1 orelse cperright= ~1) andalso vperright= ~1) then ( Queue.enqueue(fx,fposx); Queue.enqueue(fy,fposy+1); Queue.enqueue(ft,tempt+1);Queue.enqueue(cldby,(fposx,fposy))  )
						else();
						if (fposx-1<=n-1 andalso fposx-1>=0 andalso fposy>=0 andalso fposy<=m-1 andalso (cperup>tempt+1 orelse cperup= ~1) andalso vperup= ~1) then ( Queue.enqueue(fx,fposx-1); Queue.enqueue(fy,fposy); Queue.enqueue(ft,tempt+1); Queue.enqueue(cldby,(fposx,fposy))  )
						else();
						
						
						if (pertempsec>max) then bfs(n,m,fx,fy,ft,cldby,TwoDMap.insert(visit,(fposx,fposy), 1) ,timesforbfs,pertempsec,fposx,fposy,TwoDMap.insert(calledby,(fposx,fposy),caller) )
					
						else if (pertempsec=max) then (
							if (fposx<maxx) then bfs (n,m,fx,fy,ft,cldby,TwoDMap.insert(visit,(fposx,fposy),1),timesforbfs,max,fposx,fposy,TwoDMap.insert(calledby,(fposx,fposy),caller))
							else if (fposx=maxx andalso fposy<maxy) then bfs(n,m,fx,fy,ft,cldby,TwoDMap.insert(visit,(fposx,fposy),1),timesforbfs,max,fposx,fposy,TwoDMap.insert(calledby,(fposx,fposy),caller))
							else bfs(n,m,fx,fy,ft,cldby,TwoDMap.insert(visit,(fposx,fposy),1),timesforbfs,max,maxx,maxy,TwoDMap.insert(calledby,(fposx,fposy),caller))
						)
					
						else bfs(n,m,fx,fy,ft,cldby,TwoDMap.insert(visit,(fposx,fposy),1),timesforbfs,max,maxx,maxy,TwoDMap.insert(calledby,(fposx,fposy),caller))		 
					)
					else bfs(n,m,fx,fy,ft,cldby,visit,timesforbfs,max,maxx,maxy,calledby)
				end
		 	else (max,maxx,maxy,calledby)
		 	
			val fillfirstx = Queue.head pinax
			val fillfirsty = Queue.head pinay
		 		
	in
	 Queue.enqueue(fx,fillfirstx);
	 Queue.enqueue(fy,fillfirsty);
	 Queue.enqueue(ft,0);
	 Queue.enqueue(cldby,(fillfirstx,fillfirsty));
	 let
	 	val (mt,mx,my,aa)=  bfs(n,m,fx,fy,ft,cldby,TwoDMap.empty,timesforbfs,0,0,0,TwoDMap.empty);
	 	fun retrace (max,maxx,maxy,fillfirstx,fillfirsty,calledby , (x,y), list1) =
			 let
				val prev = getOpt(TwoDMap.find(calledby,(x,y)),(~1,~1))
			 in
				if ( prev = (x+1,y) ) 	   then retrace(max,maxx,maxy,fillfirstx,fillfirsty,calledby ,(x+1,y),"U"::list1)
				else if ( prev = (x-1,y) ) then retrace(max,maxx,maxy,fillfirstx,fillfirsty,calledby ,(x-1,y),"D"::list1)
				else if ( prev = (x,y+1) ) then retrace(max,maxx,maxy,fillfirstx,fillfirsty,calledby ,(x,y+1),"L"::list1)
				else if ( prev = (x,y-1) ) then retrace(max,maxx,maxy,fillfirstx,fillfirsty,calledby ,(x,y-1),"R"::list1)
				else if ( prev = (x,y) ) then (max,maxx,maxy,fillfirstx,fillfirsty,list1)
				else if ( prev = (~1,~1) ) then (max,maxx,maxy,fillfirstx,fillfirsty,"error_does_not_exist"::list1)
				else (max,maxx,maxy,fillfirstx,fillfirsty,"unknown_error"::list1)
			 end
	 in
	 	retrace (mt,mx,my,fillfirstx,fillfirsty,aa,(mx,my),[])
	 end
	end							
end

fun savethecat fileName=
	let
		val (max,maxx,maxy,fillfirstx,fillfirsty,r)= plim (fileName)
	in
		if (max = 9999999) then print("infinity")
		else print(Int.toString max);
		print("\n");
		let
			fun loop(bl)=
				let
					val hdl= hd bl
				in	 
					print(hdl);
					if (tl bl <> []) then loop(tl bl)
					else()
				end
		in
		 if (maxx=fillfirstx andalso maxy = fillfirsty) then print("stay")
		 else loop(r);
		 print("\n")
		end 
	end
	




	program test_loop
	
	integer count
	real in

	count=0
	open(11,file="temp.dat",status="old")

	do i=1,50
	read(11,*,end=100) in
	count=count+1
	end do
	
	
100	close(11)
	print*,'The count was ', count,'and the entry was ',in
	

	end
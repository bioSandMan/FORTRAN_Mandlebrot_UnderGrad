c******************************************************************
c	Calculate statistics for a column of numbers
c	input file is named temp.dat
c******************************************************************
	program avgtmp
c
	integer cnt, io
	real rms, temp, sum, in, avg
c
c	Initialize values and open temp.dat 
c
	sum=0
	cnt=0
	rms=0.0
	io=0
	open(11,file="temp.dat",status="old")
c
c	Open the temperature file and read as long as io is not 
c	at the end of file.
c
	do
	read(11,*,iostat=io) in
	    if (io > 0) then
		write(*,*)'Check input.  Something was wrong'
		exit
	    else if (io < 0) then
		write(*,*) 'The count is ',cnt
		exit
	    else
		cnt = cnt + 1
		sum = sum + in
		rms = rms + (in*in)
	    END if
	END do
	close(11)
c
c	Calculate some statistics
c
	avg = sum/cnt
	rms = sqrt(rms/cnt)
	write(*,*) 'The mean is ', avg
	write(*,*) 'The RMS is', rms

	END	
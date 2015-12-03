c******************************************************************
!	This program will read in a list of temperatures from a file
!	temp.dat and create an array, TEMP(I). The program will then
!	count the temperatures, find the mean value, and print a list
!	of temperatures which are greater than the mean. Temp.dat
!	elements must be in celcius.
!	Initialize c=-273.15 since this is absolute zero and 
!	there should not be a temperature below this.  Initialize
!	temp and grt to below absolute zero since the arrays may
!	have more elements than the file does.  This program
!	should be able to read in any temp.dat file of up to 
!	1000 elements and still be able to manipulate the data
!	without user interference.
!
!	Written by: Chris Mirabzadeh
!	Feb. 13, 2010
!	
c******************************************************************
	program avgtmp

	integer io, cnt, nphys, n
	parameter (nphys = 1000)
	real in, avg, res, temp(nphys), grt(nphys)

!Initialize values and open temp.dat to be read.

	io=0
	cnt=0
	c=-273.15
	temp=-274
	grt=-274

	open(11,file="temp.dat",status="old")

!Loop elements into temp array with an implied loop

	read(11,*,end=100)(temp(i), i=1,nphys)

!Sum all the elements in temp array

100	res=sum(temp,temp.GT.c)

!Count the number of elements in temp.dat

	cnt=count(temp.GT.c)

!Find the average temp

	avg=res/cnt

!Find elements greater than the mean and place them into
!a different array

	grt=pack(temp, mask=temp.GT.avg)

!count elements elements greater than avg since the array 
!probably has more elements than the file has.

	n=count(temp.GT.avg)

!Close the file, print some statistics and end the program

	close(11)
	write(*,*)"The number of elements is ", cnt
	write(*,*)"The sum of all elements is ", res
	write(*,*)"The mean value of the elements is ",avg
	write(*,*)"The temperatures greater than the mean are " 
	do i=1, n
	write(*,*) i,grt(i)
	end do

	END
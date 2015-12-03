!***********************************************************************
!	This program prompts the user for a number of iterations
!	then computes the value of 1/k! for k>0.
!
!	Reference:
!	http://publib.boulder.ibm.com
!***********************************************************************
	program demofactorial

	integer n, fact
	real sum, a

!Prompt user for number of iterations

100	write(*,*)"This program will compute the partial sum of e"
	write(*,*)"given N partial sums.  N cannot be greater than 32."
	write(*,*)"What is n?"
	read*, n

!Some error checking

	if (n>32) then
	write(*,*)"************************************************* "
	write(*,*)"Too many, n cannot be greater than 32.  Try again."
	write(*,*)"************************************************* "
	go to 100
	end if

!Do loop to sum up the partial sums and call the function fact(n)
!to compute the factorial of the denominator of the series function

	sum=1
	do i=1,n
	 a=fact(i)
	 sum=sum+(1/a)
	end do

!Print out the results of the above function

	Write(*,*)"The",n,"partial sums for e is", sum
	
	end

	function fact(n)
	integer fact, p,n
	p=1
	do i=1,n
	 p=p*i
	end do
	fact=p
	end
c*************************************************************
c	program to compute the sum from range given by user
c
c*************************************************************
	program sum_x
c
	integer n1, n2
c
c	ask user for range
c
	write(*,*)'Lets add a range of numbers shall we?'
	write(*,*)'What is the first number in the range?'
	read(*,*) n1
	write(*,*)'And the last number in the range?'
	read(*,*) n2
c
c	let's do this with a simple do loop
c
	x = 0
	do i=n1, n2
		x = x + i
	end do
c
c	print the value to the screen
c
	write(*,*)'The sum from ', n1,'to ', n2, 'is', x

c	that was fun

	END
c*************************************************************
c	program to compute the sum from 1 to 100
c
c*************************************************************
	program sum
c
	real x
c
c	let's do this with a simple do loop
c
	x = 0
	do i=1, 100
		x = x + i
	end do
c
c	print the value to the screen
c
	write(*,*)'The sum from 1 to 100 is', x

c	that was fun

	END

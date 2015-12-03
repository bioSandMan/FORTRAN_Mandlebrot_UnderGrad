C************************************************************
C	Circle
C	This program calculates the area of a circle based
C	on the user input when the program asks for "radius"
C************************************************************
        program circle
c
        real area, pi
	integer r
c
	pi=3.14
        print*,'Give integer radius r in ft:'
        read*, r
        area = pi*(r**2)
	print*,'The area of a circle with radius ', r,'ft. is'
	print*,area
c
	END
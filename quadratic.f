c****************************************************************************
c	This program solves quadratic equations using the quadratic formula.
c	The discriminant disc=b**2-4*A*C is checked, and if it is positive,
c	the pair of real roots, x1 and x2, is calculated; otherwise, a message
c	is displayed indicating that there are no real roots.
c
c	Written by: Chris Mirabzadeh
c****************************************************************************
       
	program quadratic

!Create Variables

	real a,b,c,disc,x, x1, x2

!Ask for coefficients

        write(*,*)"Enter coefficients A, B, C."
	write(*,*)"If A=0, the equation is not quadratic."
        read*, a,b,c

!Calculate the discriminate

        disc=b**2-4*a*c
       		
        if(disc<0) then         
                write(*,*) "The discriminant is", disc
		write(*,*) "There are no real roots"
        else                               
                disc = SQRT(disc)
                x1 = (-b + disc)*0.5/a
                x2 = (-b - disc)*0.5/a
                write(*,*)"x1 = ",x1
                write(*,*)"x2 = ",x2
        endif
	
	END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!	Fractal Geometry V. 1.0. This program will repeatedly apply the Mandelbrot
!!	function and graph the results as a fractal.
!!	
!!	Z=Z**2 + C where both Z and C are complex numbers 
!!	in the mandlebrot fractal C is the same point you are iterating (x is i and y is r or viceversa),
!!	for each point you run this iteration over and over again each time
!!	testing its distance from the origin. If the distance is greater than somevalue (usually around 2) then 	
!!	point is not it the set. If the point is iterated the maximum amount of times and is still within the	
!!	distance, it is	considered within the set. The colors are usually based on how many iterations it takes to
!!	"escape". 
!!
!!	Chris Mirabzadeh
!!	Phys 4950:  Fortran
!!	Dr. Barnbaum
!!	Spring 2010
!!
!!	References used
!!	http://en.wikipedia.org/wiki/Mandelbrot_set
!!	http://www.hku.hk/cc/sp2/software/hpf/Course/HTMLQuestionsnode56.html
!!	http://www.mps.mpg.de/dislin/color3d_c.html#section_2
!!	http://www.tat.physik.uni-tuebingen.de/~kley/lehre/ftn77/tutorial/loops.html
!!	http://www.mun.ca/hpc/hpf_pse/manual/hpf0020.htm
!!	http://www.star.le.ac.uk/~cgp/dislinGUI.html
!!	http://www.freesoftwaremagazine.com/articles/cool_fractals_with_perl_pdl_a_benchmark
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	program mandv1
	use dislin
	implicit none
	real :: dx,dy,x,y
	real :: zmat(800,800)
	real :: xscl(2), yscl(2), zscl(2)
	integer :: i, j, n, itmax,it
	complex :: c,z

!! initialize variables

	n=800
	itmax=5000
	xscl(1) = -2
	xscl(2) = 2
	yscl(1) = -2
	yscl(2) = 2
	zscl(1) = 1
	zscl(2) = itmax

!! determine dx, dy for grid based on initial values
	dx = (xscl(2) - xscl(1))/(n-1)

	dy = (yscl(2) - yscl(1))/(n-1)


!! loop over the grid
	do i = 1, n

	  do j = 1, n

!! find x,y and set the c to the location in the complex plane

      	  x = (i-1) * dx + xscl(1)
	  y = (j-1) * dy + yscl(1)
	  c = cmplx(x,y)

	  z = c

!! determine the number of iterations needed 
      
	  it = 0
      
	    do while (it < itmax .and. abs(z) < 2)
        
	    it = it + 1
        
	    z = z*z + c
	    enddo


	    zmat(i,j) = it
	  end do
	end do

!! call dislin to draw the image
	call metafl ("gif")
	call scrmod ("revers")
	call disini
	call pagera
	call hwfont
	call axspos (300, 1900)
	call ax3len (2200,1700,1700)

	call name ("Real Parts", "x")
	call name ("Imaginary Parts", "y")
	call name ("Number of Iterations", "z")
	call axsscl ("log", "z")
	call labels ("log", "z")
	call titlin ("The Mandelbrot Set", 4)
	call setscl (xscl, 2, "x")
	call setscl (yscl, 2, "y")
	call setscl (zscl, 2, "z")
	call graf3 (-2., 1., -2., 0.5, -1., 1., -1., 0.5, 0., 100., 10., 10.)
	call crvmat (zmat, n, n, 1, 1)
	call htitle (45)
	call title 
	call disfin

	end program mandv1
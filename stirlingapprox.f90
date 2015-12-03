!**********************************************************************
!Computer Problem 2
!
!1. Compute Stirling's approxiamtion for N!(1,100) into an array.
!2. Compute N!(1,100) with a do loop into a second array.
!3. Compute relative error between 1 and 2 into third array.
!4. Find the smallest integer value of N in Stirling's approx with 
!   less than 0.5% error.
!5. Output the two arrays into a file to be plotted outside
!   of this code.
!
!compiled with "gfortran -g -ffree-form"
!note to self: this would have been easier with matlab...
!**********************************************************************

!Begin Program
   program Factor

!Declare Variables
   implicit none		!Declare every variable
   integer, parameter :: n=100
   integer :: i, d

!Going to be working with large numbers so we need
!double precision.
   real*8, dimension(n) :: x, y, aerror, rerror
   real*8 fact, stirn, a, b, c, e, f

!Open a file for reading and/or writing, replace if exists.
   open(10,file="out.dat",status="replace")
   open(11,file="temp.dat",status="replace")


!Create a format for numerical output of 6 decimal places
!and a fixed format for percentage and play around with
!the output.
10  format(I4, 4E20.6)
11  format(1X, 'with less than 0.5% error is', I3, ' at', F3.2, '%')

!Create a single do-loop to process all recursive calculations.
   do i=1, n

!Compute Stirling's approxiamtion for N!(1,100) into an array.
     a=stirn(i)
     x(i)=a

!Compute N!(1,100) into a second array
     b=fact(i)
     y(i)=b

!Compute the absolute error between 1 and 2
     c=x(i)-y(i)
     aerror(i)=abs(c)

!Compute the relative error between 1 and 2.
     f=aerror(i)/y(i)
     rerror(i)=f

!Loop elements of x, y, and error into temp.dat.
!Each execution of a write command writes to a single line in the file.
     write(11,*) i, 100*rerror(i), 100*aerror(i)
   end do

!Give some output to report
   write(10,*) " # N         True Value         Approx. Value       Relative Error      Absolute Error"
   do i=1, 10
    write(10,10) i, y(i), x(i), rerror(i), aerror(i)
   end do
   write(10,*) " "
   write(10,*) "Thus, you can see that the relative error decreases as the absolute error increases."

!Find the smallest integer value of N in Stirling's approx with 
!less than 0.5% error.
   do i=1,n
     if (d.eq.1) then
      exit
     else
      if (rerror(i).lt.0.005) then
       write(*,*) 'Computer Problem 2.b'
       write(*,*) 'The smallest interger value of N in Stirlings approximation'
       write(*,11) i, 100*rerror(i)
       d=1
      end if
    end if
   end do

!Close the file before we end
   close(11)

!Create files for gnuplot to use.
!gnuplot plotmX.out
   open(12,file="plotm1.out",status="replace")
   open(13,file="plotm2.out",status="replace")

   write(12,*) "set terminal jpeg"
   write(12,*) "set output 'graph1.jpg'"
   write(12,*) "set autoscale"
   write(12,*) "unset grid"
   write(12,*) "unset log"
   write(12,*) "unset label"
   write(12,*) "unset key"
   write(12,*) "set size ratio 0.5"
   write(12,*) "set title 'Relative Error due to Strilings Approximation.'"
   write(12,*) "set xlabel 'N from 1 to 100'"
   write(12,*) "set ylabel 'Percent of error.'"
   write(12,*) "plot 'temp.dat' using 1:2 with lines"

   write(13,*) "set terminal jpeg"
   write(13,*) "set output 'graph2.jpg'"
   write(13,*) "set autoscale"
   write(13,*) "set xrange [50:150]"
   write(13,*) "unset grid"
   write(13,*) "unset log"
   write(13,*) "unset label"
   write(13,*) "unset key"
   write(13,*) "set size ratio 0.5"
   write(13,*) "set title 'Absolute Error due to Strilings Approximation.'"
   write(13,*) "set xlabel 'N from 50 to 150'"
   write(13,*) "set ylabel 'Percent of error.'"
   write(13,*) "plot 'temp.dat' using 1:3 with lines"

   close(12)
   close(13)

!Issue a command to the system that will startup GNUPLOT, using
!the file we just wrote as input.
   call system ("gnuplot plotm1.out")
   call system ("gnuplot plotm2.out")

!Every program has to end
   END

!Create a function to compute Stirling's Approx.
   function stirn(n)
    integer n
    real, parameter :: pi=3.1415926535
    real, parameter :: e=2.7182818285
    real*8 stirn
    stirn=sqrt(2.*pi*n)*(n/e)**n
   END

!Create a function to computes the factorial.
   function fact(n)
    integer n, p
    real*8 x, fact
    if (n.gt.12) then
     x=n
     fact = gamma(x+1)
    else
     p=1
    do i=1,n
     p=p*i
    end do
    fact=p
   end if
   end

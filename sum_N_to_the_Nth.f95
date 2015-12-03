!**********************************************************************
!Computer Problem 1.b
!Write a program to estimate n^n from 1 to 50 using a loop structure
!The program should print the answer to the screen in scientific
!notation with exactly 6 decimal places of accuracy.
!
!compiled with "gfortran -g -ffree-form"
!**********************************************************************

! Begin program
    program simple_loop
! Declare global variables
    implicit none	!Declare every varialbe
    integer n		!n is for counting
    real x, y		!x and y will hold values
! Initialize the variables
    n=0
    x=0
    y=0
! Simple do loop to do the work
    do n=1, 50
      x=y+n**n
      y=x
    end do
! Create a format for scientific notation with 6 decimal places
10  format(ES20.6)
! Print the final value of x to the screen with format
    write(*,10) y
! Every program must end
    END

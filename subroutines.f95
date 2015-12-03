subroutine linspace(base,limit,n)
!**********************************************************************
! Purpose:
! To function like linspace() from matlab to return an array
!
! Reference:Matlab linspace()
!
!**********************************************************************
   real*8 :: base(:)
   integer :: limit, n
   integer :: k, i, d
   k = SIZE(z); d = (n-l)/k
   base(1) = real(l)
   do i = 2,k
    base(i) = base(i-1) + d
   end do
end subroutine linspace

subroutine logspace(base, limit, n,h)
!**********************************************************************
! Purpose:
! To function like logspace() from matlab to return an array,h, of
! values logarithmically spaced from 10^base to 10^limit.
!
! Reference:Matlab logspace()
!
!**********************************************************************
   integer		:: base,limit,n
   real*8, dimension(n) :: expont,h

   expont=(/(i, i=base,0,1)/)
   h=(/(10.0**expont(i), i=1,n,1)/)

!  call logspace(base,limit,n,h)
end subroutine logspace


function stirn(n)
!Create a function to compute Stirling's Approx.
    integer n
    real, parameter :: pi=3.1415926535
    real, parameter :: e=2.7182818285
    real*8 stirn
    stirn=sqrt(2.*pi*n)*(n/e)**n
end function stirn


function fact(n)
!Create a function to computes the factorial.
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
end function fact

subroutine derivative(vector,deriv,nsamp,dx)
!**********************************************************************
!
! Purpose:
! To calculate the derivative of sampled function f(x)
! consisting of nsamp samples spaced a distance dx apart.
! The resulting derivative is returned in array deriv, and
! is nsamp-1 samples long. 
!
! Reference: Fortran 90/95 for Scientists and Engineers
!**********************************************************************
   implicit none					! Declare every variable
   integer				:: nsamp 	! number of samples
   real,dimension(nsamp)		:: vector 	! input data array
   real,dimension(nsamp)		:: deriv 	! input data array
   real				:: dx 		! Step size
   integer 			:: i		! Loop index

   do i=1, nsamp-1
    deriv(i)=(vector(i+1)-vector(i))/dx
   end do
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer d
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  integer values(8)
  integer y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

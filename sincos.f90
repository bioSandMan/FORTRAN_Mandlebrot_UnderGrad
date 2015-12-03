      PROGRAM CURVE
      USE DISLIN
      IMPLICIT NONE
      INTEGER, PARAMETER :: N=100
      REAL, DIMENSION (N) :: XRAY,Y1RAY,Y2RAY
      REAL, PARAMETER :: PI=3.1415926
      REAL :: FPI,STEP,X
      INTEGER :: I

      FPI=PI/180.
      STEP=360./(N-1)

      DO I=1,N
        XRAY(I)=(I-1)*STEP
        X=XRAY(I)*FPI
        Y1RAY(I)=SIN(X)
        Y2RAY(I)=COS(X)
      END DO
      
      CALL METAFL ('jpg')
      CALL DISINI()
      CALL PAGERA()
      CALL HWFONT()
      CALL AXSPOS(450,1800)
      CALL AXSLEN(2200,1200)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')

      CALL LABDIG(-1,'X')
      CALL TICKS(10,'XY')

      CALL TITLIN('Demonstration of CURVE',1)
      CALL TITLIN('SIN(X), COS(X)',3)

      CALL GRAF(0.,360.,0.,90.,-1.,1.,-1.,0.5)
      CALL TITLE()

      CALL COLOR('RED')
      CALL CURVE(XRAY,Y1RAY,N)
      CALL COLOR('GREEN')
      CALL CURVE(XRAY,Y2RAY,N)

      CALL COLOR('FORE')
      CALL DASH()
      CALL XAXGIT()

      CALL DISFIN()
      END
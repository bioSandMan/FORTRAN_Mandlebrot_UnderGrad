      PROGRAM LINSYS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Program to solve a linear system using Gaussian elimination.         !

! Identifiers used are:                                                !

!     LIMROW : maximum number of rows in the matrix                    !

!     LIMCOL : maximum number of colums (LIMROW + 1) in the matrix     !

!     N      : number of equations and unknowns                        !

!     I, J   : subscripts                                              !

!     LIN    : matrix for the linear system                            !

!     X      : solution                                                !

!     SINGUL : indicates if system is (nearly) singular                !

!                                                                      !

! Input:   The number of equations, the coefficients, and the          !

!          constants of the linear system                              !

! Output:  The solution of the linear system or a message indicating   !

!          that the system is (nearly) singular                        !

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 

      INTEGER LIMROW, LIMCOL

      PARAMETER (LIMROW = 10, LIMCOL = LIMROW + 1)

      DOUBLE PRECISION LIN(LIMROW, LIMCOL), X(LIMROW)

      INTEGER N, I, J

      LOGICAL SINGUL

 



! Use subroutine GAUSS to find the solution,

! and then display the solution

 

      CALL GAUSS(LIN, LIMROW, LIMCOL, N, X, SINGUL)

      IF (.NOT. SINGUL) THEN

         PRINT *, 'SOLUTION IS'

         DO 20 I = 1, N

            PRINT 100, I, X(I)

100         FORMAT(1X, 'X(', I2, ') =', F8.3)

20       CONTINUE

      ELSE

         PRINT *, 'MATRIX IS (NEARLY) SINGULAR'

      END IF

 

      END

 

 

!!GAUSS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Subroutine to find solution of a linear system of N equations in N   !

! unknowns using Gaussian elimination, provided a unique solution      !

! exists.  The coefficients and constants of the linear system are     !

! stored in the matrix LIN, which has LIMROW rows and LIMCOL columns.  !

! If the system is singular, SINGUL is returned as true, and the       !

! solution X is undefined.  Local identifiers used are:                !                        !     I,J,K  : subscripts                                              !

!     MULT   : multiplier used to eliminate an unknown                 !

!     ASBPIV : absolute value of pivot element                         !

!     PIVROW : row containing pivot element                            !

!     EPSIL  : a small positive real value ("almost zero")             !

!     TEMP   : used to interchange rows of matrix                      !

!                                                                      !

! Accepts: Two-dimensional array LIM, integers LIMROW, LIMCOL, and N   !

! Returns: One-dimensional array X and logical value SINGUL            !

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 

      SUBROUTINE GAUSS(LIN, LIMROW, LIMCOL, N, X, SINGUL)

 

      DOUBLE PRECISION LIN(LIMROW, LIMCOL), X(LIMROW), TEMP, MULT, EPSIL

      PARAMETER (EPSIL = 1D-15)

      INTEGER N, PIVROW

      LOGICAL SINGUL

     

      SINGUL = .FALSE.

      DO 50 I = 1, N

 

!        Locate pivot element

 

         ABSPIV = ABS(LIN(I,I))

         PIVROW = I

         DO 10 K = I + 1, N

            IF (ABS(LIN(K,I)) .GT. ABSPIV) THEN

               ABSPIV = ABS(LIN(K,I))

               PIVROW = K

            END IF

10       CONTINUE

 

!        Check if matrix is (nearly) singular

 

         IF (ABSPIV .LT. EPSIL) THEN

            SINGUL = .TRUE.

            RETURN

         END IF

 

!        It isn't, so interchange rows PIVROW and I if necessary

 

         IF (PIVROW .NE. I) THEN

            DO 20 J = 1, N + 1

               TEMP = LIN(I,J)

               LIN(I,J) = LIN(PIVROW,J)

               LIN(PIVROW,J) = TEMP

20          CONTINUE

         END IF

 

!        Eliminate Ith unknown from equations I + 1, ..., N

 

         DO 40 J = I + 1, N

            MULT = -LIN(J,I) / LIN(I,I)

            DO 30 K = I, N + 1

               LIN(J,K) = LIN(J,K) +  MULT ! LIN(I,K)

30          CONTINUE

40       CONTINUE

 

50    CONTINUE

 

! Find the solutions by back substitution

 

      X(N) = LIN(N, N + 1) / LIN(N,N)

      DO 70 J = N - 1, 1, -1

         X(J) = LIN(J, N + 1)

         DO 60 K = J + 1, N

            X(J) = X(J) - LIN(J,K) ! X(K)

60       CONTINUE

         X(J) = X(J) / LIN(J,J)

70    CONTINUE

 

      END

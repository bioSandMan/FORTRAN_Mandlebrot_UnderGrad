C******************************************************
C	"Input!"
C	This program asks the user to input a character
C	then and interger and a real number and then
C	prints the values to the screen.
C******************************************************
        program input
c
c declarations
c
        integer int
        real rr
        character char
c
c use the print command to ask for input for read command
c
        print*,'Enter a single character, any character'
        read(*,*) char
        print*,'Enter a whole number from 1 - 100'
        read(*,*) int
        print*,'Now enter any real number'
        read(*,*) rr
c
c once input is read to memory display to screen
c
        print*,'You entered' ,' '  , char , int , rr
c
        END
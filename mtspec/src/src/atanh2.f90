function atanh2 ( x )

!*******************************************************************************
!
!! ATANH2 returns the inverse hyperbolic tangent of a number.
!
!  Definition:
!
!    Y = ATANH2(X) implies that
!    X = TANH(Y) = ( EXP(Y) - EXP(-Y) ) / ( EXP(Y) + EXP(-Y) )
!
!  Discussion:
!
!    Since a library function ATANH may be available on some systems,
!    this routine is named ATANH2 to avoid name conflicts.
!
!  Modified:
!
!    13 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X, the number whose inverse hyperbolic 
!    tangent is desired.  The absolute value of X should be less than 
!    or equal to 1.
!
!    Output, real ( kind = 4 ) ATANH2, the inverse hyperbolic tangent of X.
!

!********************************************************************

  implicit none

  real (4) :: atanh2
  real (4) :: x

!********************************************************************

  if ( 1.0 <= abs ( x ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ATANH2 - Fatal error!'
    write ( *, '(a)' ) '  ABS(X) must be < 1.'
    write ( *, '(a,g14.6)' ) '  Your input is X = ', x
    stop
  end if

  atanh2 = real( 0.5d0 * log( (1.0d0 + dble(x)) / (1.0d0 - dble(x)) ))

  return

end function atanh2

!--------------------------------------------------------------------


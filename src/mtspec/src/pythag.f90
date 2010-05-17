double precision function pythag ( a, b )

!
!  Modified
!	German Prieto
!	November 2004
!
!  Modified
!	German Prieto
!	March 2006
!	Added rpythag, the real(4) version for QI theory to use
!
!
!  PYTHAG computes SQRT ( A**2 + B**2 ) carefully.
!
!
!  Discussion:
!
!    The formula
!
!      PYTHAG = sqrt ( A**2 + B**2 )
!
!    is reasonably accurate, but can fail if, for example, A**2 is larger 
!    than the machine overflow.  The formula can lose most of its accuracy 
!    if the sum of the squares is very large or very small.
!
!  Modified:
!
!    04 February 2003
!
!  Reference:
!
!    J H Wilkinson and C Reinsch,
!    Handbook for Automatic Computation,
!    Volume II, Linear Algebra, Part 2,
!    Springer Verlag, 1971.
!
!    B Smith, J Boyle, J Dongarra, B Garbow, Y Ikebe, V Klema, C Moler,
!    Matrix Eigensystem Routines, EISPACK Guide,
!    Lecture Notes in Computer Science, Volume 6,
!    Springer Verlag, 1976.
!
!  Modified:
!
!    04 February 2003
!
!  Parameters:
!
!    Input:
!    	a   
!    	b  	- the two legs of a right triangle.
!
!    Output:
!    
!    	pythag	- the length of the hypotenuse.
!

!********************************************************************

   implicit none

   real(8) :: a, b, p, r, s, t, u

!********************************************************************

  p = max ( abs ( a ), abs ( b ) )

  if ( p /= 0.0d0 ) then

    r = ( min ( abs ( a ), abs ( b ) ) / p )**2

    do

      t = 4.0d0 + r

      if ( t == 4.0d0 ) then
        exit
      end if

      s = r / t
      u = 1.0d0 + 2.0d0 * s
      p = u * p
      r = ( s / u )**2 * r

    end do

  end if

  pythag = p

  return

end function pythag

!--------------------------------------------------------------------

real function rpythag(a,b)

!********************************************************************

   implicit none

   real(4) :: a, b
   real(4) :: p, r, s, t, u

!********************************************************************

  p = max ( abs ( a ), abs ( b ) )

  if ( p /= 0.0d0 ) then

    r = ( min ( abs ( a ), abs ( b ) ) / p )**2

    do

      t = 4.0 + r

      if ( t == 4.0 ) then
        exit
      end if

      s = r / t
      u = 1.0 + 2.0 * s
      p = u * p
      r = ( s / u )**2 * r

    end do

  end if

  rpythag = p

  return

end function rpythag

!--------------------------------------------------------------------




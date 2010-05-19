!--------------------------------------------------------------------
! Spline subroutines
!--------------------------------------------------------------------

subroutine spline_cubic_set ( n, t, y, ibcbeg, ybcbeg, ibcend, ybcend, ypp )

!
! SPLINE_CUBIC_SET computes the second derivatives of a piecewise cubic spline.
!
!  Discussion:
!
!    For data interpolation, the user must call SPLINE_CUBIC_SET to 
!    determine the second derivative data, passing in the data to be 
!    interpolated, and the desired boundary conditions.
!
!    The data to be interpolated, plus the SPLINE_CUBIC_SET output, 
!    defines the spline.  The user may then call SPLINE_CUBIC_VAL to 
!    evaluate the spline at any point.
!
!    The cubic spline is a piecewise cubic polynomial.  The intervals
!    are determined by the "knots" or abscissas of the data to be
!    interpolated.  The cubic spline has continous first and second
!    derivatives over the entire interval of interpolation.  
!
!    For any point T in the interval T(IVAL), T(IVAL+1), the form of
!    the spline is
!
!      SPL(T) = A(IVAL)
!             + B(IVAL) * ( T - T(IVAL) ) 
!             + C(IVAL) * ( T - T(IVAL) )**2
!             + D(IVAL) * ( T - T(IVAL) )**3
!
!    If we assume that we know the values Y(*) and YPP(*), which represent
!    the values and second derivatives of the spline at each knot, then
!    the coefficients can be computed as:
!
!      A(IVAL) = Y(IVAL)
!      B(IVAL) = ( Y(IVAL+1) - Y(IVAL) ) / ( T(IVAL+1) - T(IVAL) )
!        - ( YPP(IVAL+1) + 2 * YPP(IVAL) ) * ( T(IVAL+1) - T(IVAL) ) / 6
!      C(IVAL) = YPP(IVAL) / 2
!      D(IVAL) = ( YPP(IVAL+1) - YPP(IVAL) ) / ( 6 * ( T(IVAL+1) - T(IVAL) ) )
!
!    Since the first derivative of the spline is
!
!      SPL'(T) =     B(IVAL)
!              + 2 * C(IVAL) * ( T - T(IVAL) )
!              + 3 * D(IVAL) * ( T - T(IVAL) )**2,
!
!    the requirement that the first derivative be continuous at interior
!    knot I results in a total of N-2 equations, of the form:
!
!      B(IVAL-1) + 2 C(IVAL-1) * (T(IVAL)-T(IVAL-1)) 
!      + 3 * D(IVAL-1) * (T(IVAL) - T(IVAL-1))**2 = B(IVAL)
!
!    or, setting H(IVAL) = T(IVAL+1) - T(IVAL)
!
!      ( Y(IVAL) - Y(IVAL-1) ) / H(IVAL-1)
!      - ( YPP(IVAL) + 2 * YPP(IVAL-1) ) * H(IVAL-1) / 6
!      + YPP(IVAL-1) * H(IVAL-1)
!      + ( YPP(IVAL) - YPP(IVAL-1) ) * H(IVAL-1) / 2
!      = 
!      ( Y(IVAL+1) - Y(IVAL) ) / H(IVAL)
!      - ( YPP(IVAL+1) + 2 * YPP(IVAL) ) * H(IVAL) / 6
!
!    or
!
!      YPP(IVAL-1) * H(IVAL-1) + 2 * YPP(IVAL) * ( H(IVAL-1) + H(IVAL) )
!      + YPP(IVAL) * H(IVAL) 
!      =
!      6 * ( Y(IVAL+1) - Y(IVAL) ) / H(IVAL)
!      - 6 * ( Y(IVAL) - Y(IVAL-1) ) / H(IVAL-1)    
!
!    Boundary conditions must be applied at the first and last knots.  
!    The resulting tridiagonal system can be solved for the YPP values.
!
!  Modified:
!
!    27 January 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl de Boor,
!    A Practical Guide to Splines,
!    Springer Verlag, 1978.
!
!  Parameters:
!
!    Input, integer N, the number of data points; N must be at least 2. 
!
!    Input, real ( kind = 8 ) T(N), the points where data is specified.  
!    The values should be distinct, and increasing.
!
!    Input, real ( kind = 8 ) Y(N), the data values to be interpolated.
!
!    Input, integer IBCBEG, the left boundary condition flag:
!
!      0: the spline should be a quadratic over the first interval;
!      1: the first derivative at the left endpoint should be YBCBEG;
!      2: the second derivative at the left endpoint should be YBCBEG.
!
!    Input, real ( kind = 8 ) YBCBEG, the left boundary value, if needed.
!
!    Input, integer IBCEND, the right boundary condition flag:
!
!      0: the spline should be a quadratic over the last interval;
!      1: the first derivative at the right endpoint should be YBCEND;
!      2: the second derivative at the right endpoint should be YBCEND.
!
!    Input, real ( kind = 8 ) YBCEND, the right boundary value, if needed.
!
!    Output, real ( kind = 8 ) YPP(N), the second derivatives of 
!    the cubic spline.
!

!********************************************************************

  implicit none

  integer :: n

  real(8), dimension(3,n) :: a
  integer :: i, ibcbeg, ibcend
  real(8), dimension(n) :: t, y, ypp
  real(8) :: ybcbeg, ybcend

!********************************************************************

!
!  Check.
!
  if ( n <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SPLINE_CUBIC_SET - Fatal error!'
    write ( *, '(a)' ) '  The number of knots must be at least 2.'
    write ( *, '(a,i6)' ) '  The input value of N = ', n
    stop
  end if

  do i = 1, n-1
    if ( t(i+1) <= t(i) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPLINE_CUBIC_SET - Fatal error!'
      write ( *, '(a)' ) '  The knots must be strictly increasing, but'
      write ( *, '(a,i6,a,g14.6)' ) '  T(',  i,') = ', t(i)
      write ( *, '(a,i6,a,g14.6)' ) '  T(',i+1,') = ', t(i+1)
      stop
    end if
  end do
!
!  Set the first equation.
!
  if ( ibcbeg == 0 ) then
    ypp(1) = 0.0D+00
    a(2,1) = 1.0D+00
    a(1,2) = -1.0D+00
  else if ( ibcbeg == 1 ) then
    ypp(1) = ( y(2) - y(1) ) / ( t(2) - t(1) ) - ybcbeg
    a(2,1) = ( t(2) - t(1) ) / 3.0D+00 
    a(1,2) = ( t(2) - t(1) ) / 6.0D+00
  else if ( ibcbeg == 2 ) then
    ypp(1) = ybcbeg
    a(2,1) = 1.0D+00
    a(1,2) = 0.0D+00
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SPLINE_CUBIC_SET - Fatal error!'
    write ( *, '(a)' ) '  The boundary flag IBCBEG must be 0, 1 or 2.'
    write ( *, '(a,i6)' ) '  The input value is IBCBEG = ', ibcbeg
    stop
  end if
!
!  Set the intermediate equations.
!
  do i = 2, n-1
    ypp(i) = ( y(i+1) - y(i) ) / ( t(i+1) - t(i) ) &
           - ( y(i) - y(i-1) ) / ( t(i) - t(i-1) )
    a(3,i-1) = ( t(i) - t(i-1) ) / 6.0D+00
    a(2,i) = ( t(i+1) - t(i-1) ) / 3.0D+00
    a(1,i+1) = ( t(i+1) - t(i) ) / 6.0D+00
  end do
!
!  Set the last equation.
!
  if ( ibcend == 0 ) then
    ypp(n) = 0.0D+00
    a(3,n-1) = -1.0D+00
    a(2,n) = 1.0D+00
  else if ( ibcend == 1 ) then
    ypp(n) = ybcend - ( y(n) - y(n-1) ) / ( t(n) - t(n-1) )
    a(3,n-1) = ( t(n) - t(n-1) ) / 6.0D+00
    a(2,n) = ( t(n) - t(n-1) ) / 3.0D+00
  else if ( ibcend == 2 ) then
    ypp(n) = ybcend
    a(3,n-1) = 0.0D+00
    a(2,n) = 1.0D+00
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SPLINE_CUBIC_SET - Fatal error!'
    write ( *, '(a)' ) '  The boundary flag IBCEND must be 0, 1 or 2.'
    write ( *, '(a,i6)' ) '  The input value is IBCEND = ', ibcend
    stop
  end if
!
!  Special case:
!    N = 2, IBCBEG = IBCEND = 0.
!
  if ( n == 2 .and. ibcbeg == 0 .and. ibcend == 0 ) then

    ypp(1) = 0.0D+00
    ypp(2) = 0.0D+00
!
!  Solve the linear system.
!
  else

    call d3_np_fs ( n, a, ypp, ypp )

  end if

  return
end

!--------------------------------------------------------------------

subroutine spline_cubic_val ( n, t, y, ypp, tval, yval, ypval, yppval )

!
! SPLINE_CUBIC_VAL evaluates a piecewise cubic spline at a point.
!
!  Discussion:
!
!    SPLINE_CUBIC_SET must have already been called to define the 
!    values of YPP.
!
!    For any point T in the interval T(IVAL), T(IVAL+1), the form of
!    the spline is
!
!      SPL(T) = A 
!             + B * ( T - T(IVAL) ) 
!             + C * ( T - T(IVAL) )**2
!             + D * ( T - T(IVAL) )**3
!
!    Here:
!      A = Y(IVAL)
!      B = ( Y(IVAL+1) - Y(IVAL) ) / ( T(IVAL+1) - T(IVAL) )
!        - ( YPP(IVAL+1) + 2 * YPP(IVAL) ) * ( T(IVAL+1) - T(IVAL) ) / 6
!      C = YPP(IVAL) / 2
!      D = ( YPP(IVAL+1) - YPP(IVAL) ) / ( 6 * ( T(IVAL+1) - T(IVAL) ) )
!
!  Modified:
!
!    20 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl de Boor,
!    A Practical Guide to Splines,
!    Springer Verlag, 1978.
!
!  Parameters:
!
!    Input, integer N, the number of data values.
!
!    Input, real ( kind = 8 ) T(N), the knot values.
!
!    Input, real ( kind = 8 ) Y(N), the data values at the knots.
!
!    Input, real ( kind = 8 ) YPP(N), the second derivatives of the 
!    spline at the knots.
!
!    Input, real ( kind = 8 ) TVAL, a point, typically between T(1) and 
!    T(N), at which the spline is to be evalulated.  If TVAL lies outside 
!    this range, extrapolation is used.
!
!    Output, real ( kind = 8 ) YVAL, YPVAL, YPPVAL, the value of the spline, and
!    its first two derivatives at TVAL.
!

!********************************************************************

  implicit none

  integer n

  real (8) :: dt, h, tval, yppval, ypval, yval

  integer :: left, right

  real (8 ), dimension(n) :: t, y, ypp

!********************************************************************

!
!  Determine the interval [T(LEFT), T(RIGHT)] that contains TVAL.
!  Values below T(1) or above T(N) use extrapolation.
!
  call dvec_bracket ( n, t, tval, left, right )
!
!  Evaluate the polynomial.
!
  dt = tval - t(left)
  h = t(right) - t(left)

  yval = y(left) &
       + dt * ( ( y(right) - y(left) ) / h &
              - ( ypp(right) / 6.0D+00 + ypp(left) / 3.0D+00 ) * h &
       + dt * ( 0.5D+00 * ypp(left) &
       + dt * ( ( ypp(right) - ypp(left) ) / ( 6.0D+00 * h ) ) ) )

  ypval = ( y(right) - y(left) ) / h &
       - ( ypp(right) / 6.0D+00 + ypp(left) / 3.0D+00 ) * h &
       + dt * ( ypp(left) &
       + dt * ( 0.5D+00 * ( ypp(right) - ypp(left) ) / h ) )

  yppval = ypp(left) + dt * ( ypp(right) - ypp(left) ) / h 

  return
end

!--------------------------------------------------------------------

subroutine spline_cubic_val2 ( n, t, y, ypp, left, tval, yval, ypval, yppval )

!
! SPLINE_CUBIC_VAL2 evaluates a piecewise cubic spline at a point.
!
!  Discussion:
!
!    This routine is a modification of SPLINE_CUBIC_VAL; it allows the
!    user to speed up the code by suggesting the appropriate T interval
!    to search first.
!
!    SPLINE_CUBIC_SET must have already been called to define the
!    values of YPP.
!
!    In the LEFT interval, let RIGHT = LEFT+1.  The form of the spline is
!
!      SPL(T) = 
!          A
!        + B * ( T - T(LEFT) )
!        + C * ( T - T(LEFT) )**2
!        + D * ( T - T(LEFT) )**3
!
!    Here:
!      A = Y(LEFT)
!      B = ( Y(RIGHT) - Y(LEFT) ) / ( T(RIGHT) - T(LEFT) )
!        - ( YPP(RIGHT) + 2 * YPP(LEFT) ) * ( T(RIGHT) - T(LEFT) ) / 6
!      C = YPP(LEFT) / 2
!      D = ( YPP(RIGHT) - YPP(LEFT) ) / ( 6 * ( T(RIGHT) - T(LEFT) ) )
!
!  Modified:
!
!    24 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl de Boor,
!    A Practical Guide to Splines,
!    Springer Verlag, 1978.
!
!  Parameters:
!
!    Input, integer N, the number of knots.
!
!    Input, real ( kind = 8 ) T(N), the knot values.
!
!    Input, real ( kind = 8 ) Y(N), the data values at the knots.
!
!    Input, real ( kind = 8 ) YPP(N), the second derivatives of the spline at
!    the knots.
!
!    Input/output, integer LEFT, the suggested T interval to search.
!    LEFT should be between 1 and N-1.  If LEFT is not in this range,
!    then its value will be ignored.  On output, LEFT is set to the
!    actual interval in which TVAL lies.
!
!    Input, real ( kind = 8 ) TVAL, a point, typically between T(1) and T(N), at
!    which the spline is to be evalulated.  If TVAL lies outside
!    this range, extrapolation is used.
!
!    Output, real ( kind = 8 ) YVAL, YPVAL, YPPVAL, the value of the spline, and
!    its first two derivatives at TVAL.
!

!********************************************************************

  implicit none

  integer :: n, left, right

  real(8) dt, h, tval, yppval, ypval, yval

  real(8), dimension(n) :: t, y, ypp

!********************************************************************

!
!  Determine the interval [T(LEFT), T(RIGHT)] that contains TVAL.
!  
!  What you want from DVEC_BRACKET3 is that TVAL is to be computed
!  by the data in interval {T(LEFT), T(RIGHT)].
!
  call dvec_bracket3 ( n, t, tval, left )
  right = left + 1
!
!  In the interval LEFT, the polynomial is in terms of a normalized
!  coordinate  ( DT / H ) between 0 and 1.
!
  dt = tval - t(left)
  h = t(right) - t(left)

  yval = y(left) + dt * ( ( y(right) - y(left) ) / h &
              - ( ypp(right) / 6.0D+00 + ypp(left) / 3.0D+00 ) * h &
       + dt * ( 0.5D+00 * ypp(left) &
       + dt * ( ( ypp(right) - ypp(left) ) / ( 6.0D+00 * h ) ) ) )

  ypval = ( y(right) - y(left) ) / h &
      - ( ypp(right) / 6.0D+00 + ypp(left) / 3.0D+00 ) * h &
      + dt * ( ypp(left) &
      + dt * ( 0.5D+00 * ( ypp(right) - ypp(left) ) / h ) )

  yppval = ypp(left) + dt * ( ypp(right) - ypp(left) ) / h

  return
end

!--------------------------------------------------------------------

subroutine d3_np_fs ( n, a, b, x )

!
! D3_NP_FS factors and solves an D3 system.
!
!  Discussion:
!
!    The D3 storage format is used for a tridiagonal matrix.
!    The superdiagonal is stored in entries (1,2:N), the diagonal in
!    entries (2,1:N), and the subdiagonal in (3,1:N-1).  Thus, the
!    original matrix is "collapsed" vertically into the array.
!
!    This algorithm requires that each diagonal entry be nonzero.
!    It does not use pivoting, and so can fail on systems that
!    are actually nonsingular.
!
!  Example:
!
!    Here is how a D3 matrix of order 5 would be stored:
!
!       *  A12 A23 A34 A45
!      A11 A22 A33 A44 A55
!      A21 A32 A43 A54  *
!
!  Modified:
!
!    02 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the linear system.
!
!    Input/output, real ( kind = 8 ) A(3,N).
!    On input, the tridiagonal matrix.
!    On output, the data in these vectors has been overwritten
!    by factorization information.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!

!********************************************************************

  implicit none

  integer :: n, i

  real (8), dimension(3,n) ::  a
  real (8), dimension(n) :: b, x
  real (8) :: xmult

!********************************************************************

!
!  The diagonal entries can't be zero.
!
  do i = 1, n
    if ( a(2,i) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'D3_NP_FS - Fatal error!'
      write ( *, '(a,i6,a)' ) '  A(2,', i, ') = 0.'
      return
    end if
  end do

  x(1:n) = b(1:n)

  do i = 2, n
    xmult = a(3,i-1) / a(2,i-1)
    a(2,i) = a(2,i) - xmult * a(1,i)
    x(i)   = x(i)   - xmult * x(i-1)
  end do

  x(n) = x(n) / a(2,n)
  do i = n-1, 1, -1
    x(i) = ( x(i) - a(1,i+1) * x(i+1) ) / a(2,i)
  end do

  return
end

!--------------------------------------------------------------------

subroutine dvec_bracket ( n, x, xval, left, right )

!
! DVEC_BRACKET searches a sorted array for successive brackets of a value.
!
!  Discussion:
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    nearest to or containing the given value.
!
!  Modified:
!
!    06 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, length of input array.
!
!    Input, real ( kind = 8 ) X(N), an array sorted into ascending order.
!
!    Input, real ( kind = 8 ) XVAL, a value to be bracketed.
!
!    Output, integer LEFT, RIGHT, the results of the search.
!    Either:
!      XVAL < X(1), when LEFT = 1, RIGHT = 2;
!      X(N) < XVAL, when LEFT = N-1, RIGHT = N;
!    or
!      X(LEFT) <= XVAL <= X(RIGHT).
!

!********************************************************************

  implicit none

  integer :: n, i, left, right

  real(8), dimension(n) :: x
  real(8) :: xval

!********************************************************************

  do i = 2, n - 1

    if ( xval < x(i) ) then
      left = i - 1
      right = i
      return
    end if

   end do

  left = n - 1
  right = n

  return
end

!--------------------------------------------------------------------

subroutine dvec_bracket3 ( n, t, tval, left )

!
! DVEC_BRACKET3 finds the interval containing or nearest a given value.
!
!  Discussion:
!
!    The routine always returns the index LEFT of the sorted array
!    T with the property that either
!    *  T is contained in the interval [ T(LEFT), T(LEFT+1) ], or
!    *  T < T(LEFT) = T(1), or
!    *  T > T(LEFT+1) = T(N).
!
!    The routine is useful for interpolation problems, where
!    the abscissa must be located within an interval of data
!    abscissas for interpolation, or the "nearest" interval
!    to the (extreme) abscissa must be found so that extrapolation
!    can be carried out.
!
!  Modified:
!
!    05 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, length of the input array.
!
!    Input, real ( kind = 8 ) T(N), an array sorted into ascending order.
!
!    Input, real ( kind = 8 ) TVAL, a value to be bracketed by entries of T.
!
!    Input/output, integer LEFT.
!
!    On input, if 1 <= LEFT <= N-1, LEFT is taken as a suggestion for the
!    interval [ T(LEFT), T(LEFT+1) ] in which TVAL lies.  This interval
!    is searched first, followed by the appropriate interval to the left
!    or right.  After that, a binary search is used.
!
!    On output, LEFT is set so that the interval [ T(LEFT), T(LEFT+1) ]
!    is the closest to TVAL; it either contains TVAL, or else TVAL
!    lies outside the interval [ T(1), T(N) ].
!

!********************************************************************

  implicit none

  integer :: n, high, left, low, mid

  real(8), dimension(n) :: t
  real(8) :: tval

!********************************************************************

!
!  Check the input data.
!
  if ( n < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DVEC_BRACKET3 - Fatal error!'
    write ( *, '(a)' ) '  N must be at least 2.'
    stop
  end if
!
!  If LEFT is not between 1 and N-1, set it to the middle value.
!
  if ( left < 1 .or. n - 1 < left ) then
    left = ( n + 1 ) / 2
  end if
!
!  CASE 1: TVAL < T(LEFT):
!  Search for TVAL in [T(I), T(I+1)] for intervals I = 1 to LEFT-1.
!
  if ( tval < t(left) ) then

    if ( left == 1 ) then
      return
    else if ( left == 2 ) then
      left = 1
      return
    else if ( t(left-1) <= tval ) then
      left = left - 1
      return
    else if ( tval <= t(2) ) then
      left = 1
      return
    end if
!
!  ...Binary search for TVAL in [T(I), T(I+1)] for intervals I = 2 to LEFT-2.
!
    low = 2
    high = left - 2

    do

      if ( low == high ) then
        left = low
        return
      end if

      mid = ( low + high + 1 ) / 2

      if ( t(mid) <= tval ) then
        low = mid
      else
        high = mid - 1
      end if

    end do
!
!  CASE2: T(LEFT+1) < TVAL:
!  Search for TVAL in {T(I),T(I+1)] for intervals I = LEFT+1 to N-1.
!
  else if ( t(left+1) < tval ) then

    if ( left == n - 1 ) then
      return
    else if ( left == n - 2 ) then
      left = left + 1
      return
    else if ( tval <= t(left+2) ) then
      left = left + 1
      return
    else if ( t(n-1) <= tval ) then
      left = n - 1
      return
    end if
!
!  ...Binary search for TVAL in [T(I), T(I+1)] for intervals I = LEFT+2 to N-2.
!
    low = left + 2
    high = n - 2

    do

      if ( low == high ) then
        left = low
        return
      end if

      mid = ( low + high + 1 ) / 2

      if ( t(mid) <= tval ) then
        low = mid
      else
        high = mid - 1
      end if

    end do
!
!  CASE3: T(LEFT) <= TVAL <= T(LEFT+1):
!  T is in [T(LEFT), T(LEFT+1)], as the user said it might be.
!
  else

  end if

  return
end






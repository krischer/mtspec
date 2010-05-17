subroutine tridib ( n, eps1, d, e, e2, lb, ub, m11, m, w, ind, ierr )

!
!  Modified
!	German Prieto
!	November 2004
!
!  TRIDIB computes some eigenvalues of a real symmetric tridiagonal matrix.
!
!
!  Discussion:
!
!    This subroutine finds those eigenvalues of a tridiagonal
!    symmetric matrix between specified boundary indices,
!    using bisection.
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
!  Parameters:
!
!    Input, integer N, the order of the matrix.
!
!    Input/output, real EPS1.  On input, an absolute error tolerance for 
!    the computed eigenvalues.  It should be chosen commensurate with
!    relative perturbations in the matrix elements of the order of the 
!    relative machine precision.  If the input EPS1 is non-positive, it 
!    is reset for each submatrix to a default value, namely, minus the
!    product of the relative machine precision and the 1-norm of the submatrix.
!
!    Input, real D(N), the diagonal elements of the input matrix.
!
!    Input, real E(N), the subdiagonal elements of the input matrix
!    in E(2:N).  E(1) is arbitrary.
!
!    Input/output, real E2(N).  On input, the squares of the corresponding 
!    elements of E.  E2(1) is arbitrary.  On output, elements of E2 
!    corresponding to elements of E regarded as negligible, have been 
!    replaced by zero, causing the matrix to split into a direct sum of 
!    submatrices.  E2(1) is also set to zero.
!
!    Input, integer M11, the lower boundary index for the desired eigenvalues.
!
!    Input, integer M, the number of eigenvalues desired.  The upper
!    boundary index M22 is then obtained as M22 = M11 + M - 1.
!
!    Output, real LB, UB, define an interval containing exactly the desired
!    eigenvalues.
!
!    Output, real W(M), the eigenvalues between indices M11 and M22 
!    in ascending order.
!
!    Output, integer IND(M), the submatrix indices associated with the 
!    corresponding eigenvalues in W: 1 for eigenvalues belonging to the 
!    first submatrix from the top, 2 for those belonging to the second 
!    submatrix, and so on.
!
!    Output, integer IERR, error flag.
!    0, for normal return,
!    3*N+1, if multiple eigenvalues at index M11 make unique selection 
!      impossible,
!    3*N+2, if multiple eigenvalues at index M22 make unique selection
!      impossible.
!

!********************************************************************

  implicit none

  integer, intent( in ) :: n
  integer :: m, i, ierr, ii, isturm, j, k, l
  integer :: m1, m11, m2, m22, p, q, r, s, tag 
  integer, dimension(m) :: ind
  
  real(8) :: eps1, lb, t1, t2, tst1, tst2
  real(8) :: u, ub, v, x0, x1, xu
  
  real(8), dimension(n) :: d, e, e2, rv4, rv5
  real(8), dimension(m) :: w 

!********************************************************************

  ierr = 0
  tag = 0
  xu = d(1)
  x0 = d(1)
  s = 0
  u = 0.0d0
!
!  Look for small sub-diagonal entries and determine an
!  interval containing all the eigenvalues.
!

  do i = 1, n

     x1 = u

     if ( i == n ) then
       u = 0.0d0
     else
       u = abs ( e(i+1) )
     end if

     xu = min ( xu, d(i)-(x1+u) )
     x0 = max ( x0, d(i)+(x1+u) )

     if ( i > 1 ) then
       tst1 = abs ( d(i) ) + abs ( d(i-1) )
       tst2 = tst1 + abs ( e(i) )
       if ( tst2 <= tst1 ) then
         e2(i) = 0.0d0
       end if
     else
       e2(i) = 0.0d0
     end if

  end do

  x1 = dble(n)
  x1 = x1 * max ( abs ( xu ), abs ( x0 ) ) * epsilon ( x1 )
  xu = xu - x1
  t1 = xu
  x0 = x0 + x1
  t2 = x0
!
!  Determine an interval containing exactly the desired eigenvalues.
!
  p = 1
  q = n
  m1 = m11 - 1
  if ( m1 == 0 ) go to 75
  isturm = 1

50 continue

  v = x1
  x1 = xu + (x0 - xu) * 0.5d0
  if ( x1 == v ) go to 980
  go to 320

60 continue

  if ( s - m1 ) 65, 73, 70

65 continue

  xu = x1
  go to 50

70 continue

  x0 = x1
  go to 50

73 continue

  xu = x1
  t1 = x1

75 continue

  m22 = m1 + m
  if ( m22 == n ) go to 90
  x0 = t2
  isturm = 2
  go to 50

80 continue

   if ( s - m22 ) 65, 85, 70

85 continue

   t2 = x1

90 continue

  q = 0
  r = 0
!
!  Establish and process next submatrix, refining interval by the 
!  Gerschgorin bounds.
!
100 continue

  if ( r == m ) then
    go to 1001
  end if

  tag = tag + 1
  p = q + 1
  xu = d(p)
  x0 = d(p)
  u = 0.0d0

  do q = p, n

    x1 = u
    u = 0.0d0
    v = 0.0d0

    if ( q < n ) then
      u = abs ( e(q+1) )
      v = e2(q+1)
    end if

    xu = min ( d(q)-(x1+u), xu )
    x0 = max ( d(q)+(x1+u), x0 )

    if ( v == 0.0d0 ) then
      exit
    end if

  end do

  x1 = max ( abs ( xu ), abs ( x0 ) ) * epsilon ( x1 )
  if ( eps1 <= 0.0d0 ) eps1 = -x1
  if ( p /= q ) go to 180
!
!  Check for isolated root within interval.
!
  if ( t1 > d(p) .or. d(p) >= t2 ) go to 940
  m1 = p
  m2 = p
  rv5(p) = d(p)
  go to 900

180 continue

  x1 = x1 * (q - p + 1)
  lb = max ( t1, xu-x1 )
  ub = min ( t2, x0+x1 )
  x1 = lb
  isturm = 3
  go to 320

200 continue

  m1 = s + 1
  x1 = ub
  isturm = 4
  go to 320

220 continue

  m2 = s
  if ( m1 > m2 ) go to 940
!
!  Find roots by bisection.
!
  x0 = ub
  isturm = 5

  rv5(m1:m2) = ub
  rv4(m1:m2) = lb
!
!  Loop for the K-th eigenvalue.
!
  k = m2

250 continue

  xu = lb

  do ii = m1, k

    i = m1 + k - ii
    if ( xu < rv4(i) ) then
      xu = rv4(i)
      exit
    end if

  end do

  if ( x0 > rv5(k) ) x0 = rv5(k)
!
!  Next bisection step.
!
300  continue

     x1 = ( xu + x0 ) * 0.5d0
     if ( ( x0 - xu ) <= abs ( eps1) ) go to 420
     tst1 = 2.0d0 * ( abs ( xu ) + abs ( x0 ) )
     tst2 = tst1 + (x0 - xu)
     if ( tst2 == tst1 ) go to 420
!
!  Sturm sequence.
!
320  continue

     s = p - 1
     u = 1.0d0

     do i = p, q

       if ( u == 0.0d0 ) then
         v = abs ( e(i) ) / epsilon ( v )
         if ( e2(i) == 0.0d0 ) v = 0.0d0
       else
         v = e2(i) / u
       end if

       u = d(i) - x1 - v

       if ( u < 0.0d0 ) then
         s = s + 1
       end if

     end do

     go to (60,80,200,220,360), isturm
!
!  Refine intervals.
!
360  continue

     if ( s >= k) go to 400
     xu = x1
     if ( s >= m1) go to 380
     rv4(m1) = x1
     go to 300

380  continue

     rv4(s+1) = x1
     if ( rv5(s) > x1) rv5(s) = x1
     go to 300

400  continue

     x0 = x1
     go to 300
!
!  K-th eigenvalue found.
!
420  continue

  rv5(k) = x1
  k = k - 1
  if ( k >= m1 ) go to 250
!
!  Order eigenvalues tagged with their submatrix associations.
!
900 continue

  s = r
  r = r + m2 - m1 + 1
  j = 1
  k = m1

  do l = 1, r

     if ( j > s ) go to 910
     if ( k > m2 ) go to 940
     if ( rv5(k) >= w(l) ) go to 915

     do ii = j, s
       i = l + s - ii
       w(i+1) = w(i)
       ind(i+1) = ind(i)
     end do

910  continue

     w(l) = rv5(k)
     ind(l) = tag
     k = k + 1
     go to 920

915  continue

     j = j + 1

920  continue

  end do

940 continue

  if ( q < n ) go to 100

  go to 1001
!
!  Set error: interval cannot be found containing exactly the 
!  desired eigenvalues.
!
980 continue

  ierr = 3 * n + isturm

1001 continue

  lb = t1
  ub = t2
  return

end subroutine tridib



subroutine tinvit ( n, d, e, e2, m, w, ind, z, ierr )

!
!  Modified
!	German Prieto
!	November 2004
!
!  TINVIT computes eigenvectors from eigenvalues, real tridiagonal symmetric.
!
!
!  Discussion:
!
!    This subroutine finds those eigenvectors of a tridiagonal
!    symmetric matrix corresponding to specified eigenvalues,
!    using inverse iteration.
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
!    Input, real D(N), the diagonal elements of the matrix.
!
!    Input, real E(N), contains the subdiagonal elements of the input matrix
!    in E(2:N).  E(1) is arbitrary.
!
!    Input, real E2(N), contains the squares of the corresponding elements 
!    of E, with zeros corresponding to negligible elements of E.
!    E(I) is considered negligible if it is not larger than the product of 
!    the relative machine precision and the sum of the magnitudes of D(I) 
!    and D(I-1).  E2(1) must contain 0.0E+00 if the eigenvalues are in 
!    ascending order, or 2.0E+00 if the eigenvalues are in descending order.  
!    If BISECT, TRIDIB, or IMTQLV has been used to find the eigenvalues,
!    their output E2 array is exactly what is expected here.
!
!    Input, integer M, the number of specified eigenvalues.
!
!    Input, real W(M), the eigenvalues.
!
!    Input, integer IND(M), the submatrix indices associated with the 
!    corresponding eigenvalues in W: 1 for eigenvalues belonging to the 
!    first submatrix from the top, 2 for those belonging to the second 
!    submatrix, and so on.
!
!    Output, real Z(N,M), the associated set of orthonormal eigenvectors.
!    Any vector which fails to converge is set to zero.
!
!    Output, integer IERR, error flag.
!    0, for normal return,
!    -R, if the eigenvector corresponding to the R-th eigenvalue fails to 
!      converge in 5 iterations.
!

!********************************************************************

   implicit none

   integer :: m,n, group, i, ierr, ii, ip, its, j, jj
   integer :: p, q, r, s, tag 

   integer, dimension(m) :: ind
 
   real(8) :: eps2, eps3, eps4, norm, order, pythag
   real(8) :: u, uk, v, x0, x1, xu
   real(8), dimension(n) :: d, e, e2, rv1, rv2, rv3, rv4, rv6
   real(8), dimension(m) :: w
   real(8), dimension(n,m) :: z

!********************************************************************

  ierr = 0

  if ( m == 0 ) then
    return
  end if

  u = 0.0d0
  x0 = 0.0d0

  tag = 0
  order = 1.0d0 - e2(1)
  q = 0
!
!  Establish and process next submatrix.
!
100 continue

  p = q + 1

  do q = p, n
    if ( q == n ) then
      exit
    end if
    if ( e2(q+1) == 0.0d0 ) then
      exit
    end if
  end do
!
!  Find vectors by inverse iteration.
!
140 continue

  tag = tag + 1
  s = 0

  do r = 1, m

     if ( ind(r) /= tag ) go to 920

     its = 1
     x1 = w(r)

     if ( s /= 0 ) go to 510
!
!  Check for isolated root.
!
     xu = 1.0d0

     if ( p == q ) then
       rv6(p) = 1.0d0
       go to 870
     end if

     norm = abs ( d(p) )
     ip = p + 1

     do i = p+1, q
       norm = max ( norm, abs ( d(i) ) + abs ( e(i) ) )
     end do
!
!  EPS2 is the criterion for grouping,
!  EPS3 replaces zero pivots and equal roots are modified by EPS3,
!  EPS4 is taken very small to avoid overflow.
!
     eps2 = 0.001d0 * norm
     eps3 = abs ( norm ) * epsilon ( eps3 )
     uk = dble(q - p + 1)
     eps4 = uk * eps3
     uk = eps4 / sqrt ( uk )
     s = p

505 continue

     group = 0
     go to 520
!
!  Look for close or coincident roots.
!
510  continue

     if ( abs ( x1 - x0 ) >= eps2 ) go to 505

     group = group + 1

     if ( order * (x1 - x0) <= 0.0d0 ) then
       x1 = x0 + order * eps3
     end if
!
!  Elimination with interchanges and initialization of vector.
!
520  continue

     v = 0.0d0

     do i = p, q

        rv6(i) = uk

        if ( i == p ) go to 560

        if ( abs ( e(i) ) < abs ( u ) ) go to 540

        xu = u / e(i)
        rv4(i) = xu
        rv1(i-1) = e(i)
        rv2(i-1) = d(i) - x1
        rv3(i-1) = 0.0d0
        if ( i /= q ) rv3(i-1) = e(i+1)
        u = v - xu * rv2(i-1)
        v = - xu * rv3(i-1)
        go to 580

540     continue

        xu = e(i) / u
        rv4(i) = xu
        rv1(i-1) = u
        rv2(i-1) = v
        rv3(i-1) = 0.0d0

560     continue

        u = d(i) - x1 - xu * v
        if ( i /= q ) v = e(i+1)

580     continue

     end do

     if ( u == 0.0d0 ) then
       u = eps3
     end if

     rv1(q) = u
     rv2(q) = 0.0d0
     rv3(q) = 0.0d0
!
!  Back substitution.
!
600   continue

  do ii = p, q
    i = p + q - ii
    rv6(i) = ( rv6(i) - u * rv2(i) - v * rv3(i) ) / rv1(i)
    v = u
    u = rv6(i)
  end do
!
!  Orthogonalize with respect to previous members of group.
!
     j = r

     do jj = 1, group

       do

         j = j - 1

         if ( ind(j) == tag ) then
           exit
         end if

       end do

       xu = dot_product ( rv6(p:q), z(p:q,j) )

       rv6(p:q) = rv6(p:q) - xu * z(p:q,j)

     end do

     norm = sum ( abs ( rv6(p:q) ) )

     if ( norm >= 1.0d0 ) go to 840
!
!  Forward substitution.
!
     if ( its == 5 ) go to 830

     if ( norm == 0.0d0 ) then
       rv6(s) = eps4
       s = s + 1
       if ( s > q ) s = p
       go to 780
     end if

740  continue

     xu = eps4 / norm
     rv6(p:q) = rv6(p:q) * xu
!
!  Elimination operations on next vector iterate.
!
780  continue
!
!  If RV1(I-1) == E(I), a row interchange was performed earlier in the
!  triangularization process.
!
     do i = ip, q

       u = rv6(i)

       if ( rv1(i-1) == e(i) ) then
         u = rv6(i-1)
         rv6(i-1) = rv6(i)
       end if

       rv6(i) = u - rv4(i) * rv6(i-1)

     end do

     its = its + 1
     go to 600
!
!  Set error: non-converged eigenvector.
!
830  continue

     ierr = -r
     xu = 0.0d0
     go to 870
!
!  Normalize so that sum of squares is 1 and expand to full order.
!
840  continue

     u = 0.0d0
     do i = p, q
       u = pythag ( u, rv6(i) )
     end do

     xu = 1.0d0 / u

870  continue

     z(1:n,r) = 0.0d0
     z(p:q,r) = rv6(p:q) * xu

     x0 = x1

920  continue

  end do

  if ( q < n ) go to 100

  return

end subroutine tinvit




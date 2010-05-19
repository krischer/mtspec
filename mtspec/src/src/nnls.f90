subroutine nnls (a, m, n, b, x, rnorm, w, indx, mode)

!     subroutine nnls(a, m, n, b, x, rnorm, w, indx, mode)
!
!  algorithm nnls: nonnegative least squares
!
!  the original version of this code was developed by
!  charles l. lawson and richard j. hanson at jet propulsion laboratory
!  1973 jun 15, and published in the book
!  "solving least squares problems", prentice-hall, 1974.
!  revised feb 1995 to accompany reprinting of the book by siam.
!
!  this translation into fortran 90 by alan miller, february 1997
!  latest revision - 15 april 1997

!  n.b. the following call arguments have been removed:
!       mda, zz
!
!  given an m by n matrix, a, and an m-vector, b,  compute an
!  n-vector, x, that solves the least squares problem
!
!                   a * x = b  subject to x  >=  0
!  ------------------------------------------------------------------
!                  subroutine arguments
!
!  a(), m, n   on entry, a() contains the m by n matrix, a.
!              on exit, a() contains the product matrix, q*a , where q is an
!              m x m orthogonal matrix generated implicitly by this subroutine.
!  b()     on entry b() contains the m-vector, b.   on exit b() contains q*b.
!  x()     on entry x() need not be initialized.
!          on exit x() will contain the solution vector.
!  rnorm   on exit rnorm contains the euclidean norm of the residual vector.
!  w()     an n-array of working space.  on exit w() will contain the dual
!          solution vector.   w will satisfy w(i) = 0. for all i in set p
!          and w(i) <= 0. for all i in set z
!  indx()  an integer working array of length at least n.
!          on exit the contents of this array define the sets p and z
!          as follows..
!              indx(1)   thru indx(nsetp) = set p.
!              indx(iz1) thru indx(iz2)   = set z.
!              iz1 = nsetp + 1 = npp1
!              iz2 = n
!  mode    this is a success-failure flag with the following meanings.
!          1   the solution has been computed successfully.
!          2   the dimensions of the problem are bad.
!              either m <= 0 or n <= 0.
!          3   iteration count exceeded.  more than 3*n iterations.
!
!     ------------------------------------------------------------------

implicit none
integer, intent(in)       :: m, n
integer, intent(out)      :: indx(n), mode
real (8), intent(in out) :: a(m,n), b(m)
real (8), intent(out)    :: x(n), rnorm, w(n)

interface
  subroutine g1(a, b, cterm, sterm, sig)
    implicit none
    real (8), intent(in)  :: a, b
    real (8), intent(out) :: cterm, sterm, sig
  end subroutine g1

  subroutine h12(mode, lpivot, l1, m, u, up, c, ice, icv, ncv)
    implicit none
    integer, intent(in)                     :: mode, lpivot, l1, m, ice, icv, &
                                               ncv
    real (8), dimension(:), intent(in out) :: u, c
    real (8), intent(in out)               :: up
  end subroutine h12
end interface

! local variables

integer                 :: i, ii, ip, iter, itmax, iz, iz1, iz2, izmax,   &
                           j, jj, jz, l, mda, npp1, nsetp
real (8), dimension(m) :: zz
real (8), dimension(1) :: dummy
real (8)               :: alpha, asave, cc, factor = 0.01d0, sm, &
                           ss, t, temp, two = 2.0d0, unorm, up, wmax,    &
                           zero = 0.0d0, ztest
!     ------------------------------------------------------------------
mode = 1
if (m <= 0 .or. n <= 0) then
  mode = 2
  return
endif
iter = 0
itmax = 3*n

!                    initialize the arrays indx() and x().

do i = 1,n
  x(i) = zero
  indx(i) = i
enddo

iz2 = n
iz1 = 1
nsetp = 0
npp1 = 1
!                             ******  main loop begins here  ******
!                  quit if all coefficients are already in the solution.
!                        or if m cols of a have been triangularized.

30 if (iz1 > iz2 .or. nsetp >= m) go to 350

!         compute components of the dual (negative gradient) vector w().

do iz = iz1,iz2
  j = indx(iz)
  w(j) = dot_product(a(npp1:m,j), b(npp1:m))
enddo

!                                   find largest positive w(j).
60 wmax = zero
do iz = iz1,iz2
  j = indx(iz)
  if (w(j) > wmax) then
    wmax = w(j)
    izmax = iz
  endif
enddo

!             if wmax  <=  0. go to termination.
!             this indicates satisfaction of the kuhn-tucker conditions.

if (wmax <= zero) go to 350
iz = izmax
j = indx(iz)

!     the sign of w(j) is ok for j to be moved to set p.
!     begin the transformation and check new diagonal element to avoid
!     near linear dependence.

asave = a(npp1,j)
call h12 (1, npp1, npp1+1, m, a(:,j), up, dummy, 1, 1, 0)
unorm = zero
if (nsetp  /=  0) then
  unorm = sum( a(1:nsetp,j)**2 )
endif
unorm = sqrt(unorm)
if (unorm + abs(a(npp1,j))*factor - unorm  >  zero) then

!        col j is sufficiently independent.  copy b into zz, update zz
!        and solve for ztest ( = proposed new value for x(j) ).

  zz(1:m) = b(1:m)
  call h12 (2, npp1, npp1+1, m, a(:,j), up, zz, 1, 1, 1)
  ztest = zz(npp1)/a(npp1,j)

!                                     see if ztest is positive

  if (ztest > zero) go to 140
endif

!     reject j as a candidate to be moved from set z to set p.
!     restore a(npp1,j), set w(j) = 0., and loop back to test dual
!     coeffs again.

a(npp1,j) = asave
w(j) = zero
go to 60

!     the index  j = indx(iz)  has been selected to be moved from
!     set z to set p.    update b,  update indices,  apply householder
!     transformations to cols in new set z,  zero subdiagonal elts in
!     col j,  set w(j) = 0.

140 b(1:m) = zz(1:m)

indx(iz) = indx(iz1)
indx(iz1) = j
iz1 = iz1+1
nsetp = npp1
npp1 = npp1+1

mda = size(a,1)
if (iz1  <=  iz2) then
  do jz = iz1,iz2
    jj = indx(jz)
    call h12 (2, nsetp, npp1, m, a(:,j), up, a(:,jj), 1, mda, 1)
  enddo
endif

if (nsetp /= m) then
  a(npp1:m,j) = zero
endif

w(j) = zero
!                                solve the triangular system.
!                                store the solution temporarily in zz().
call solve_triangular(zz)

!                       ******  secondary loop begins here ******

!                          iteration counter.

210 iter = iter+1
if (iter > itmax) then
  mode = 3
  write (*,'(/a)') ' nnls quitting on iteration count.'
  go to 350
endif

!                    see if all new constrained coeffs are feasible.
!                                  if not compute alpha.

alpha = two
do ip = 1,nsetp
  l = indx(ip)
  if (zz(ip)  <=  zero) then
    t = -x(l)/(zz(ip)-x(l))
    if (alpha > t) then
      alpha = t
      jj = ip
    endif
  endif
enddo

!          if all new constrained coeffs are feasible then alpha will
!          still = 2.    if so exit from secondary loop to main loop.

if (alpha == two) go to 330

!          otherwise use alpha which will be between 0. and 1. to
!          interpolate between the old x and the new zz.

do ip = 1,nsetp
  l = indx(ip)
  x(l) = x(l) + alpha*(zz(ip)-x(l))
enddo

!        modify a and b and the index arrays to move coefficient i
!        from set p to set z.

i = indx(jj)
260 x(i) = zero

if (jj /= nsetp) then
  jj = jj+1
  do j = jj,nsetp
    ii = indx(j)
    indx(j-1) = ii
    call g1 (a(j-1,ii), a(j,ii), cc, ss, a(j-1,ii))
    a(j,ii) = zero
    do l = 1,n
      if (l /= ii) then

!                 apply procedure g2 (cc,ss,a(j-1,l),a(j,l))

        temp = a(j-1,l)
        a(j-1,l) = cc*temp + ss*a(j,l)
        a(j,l)   = -ss*temp + cc*a(j,l)
      endif
    enddo

!                 apply procedure g2 (cc,ss,b(j-1),b(j))

    temp = b(j-1)
    b(j-1) = cc*temp + ss*b(j)
    b(j)   = -ss*temp + cc*b(j)
  enddo
endif

npp1 = nsetp
nsetp = nsetp-1
iz1 = iz1-1
indx(iz1) = i

!        see if the remaining coeffs in set p are feasible.  they should
!        be because of the way alpha was determined.
!        if any are infeasible it is due to round-off error.  any
!        that are nonpositive will be set to zero
!        and moved from set p to set z.

do jj = 1,nsetp
  i = indx(jj)
  if (x(i) <= zero) go to 260
enddo

!         copy b( ) into zz( ).  then solve again and loop back.

zz(1:m) = b(1:m)
call solve_triangular(zz)
go to 210
!                      ******  end of secondary loop  ******

330 do ip = 1,nsetp
  i = indx(ip)
  x(i) = zz(ip)
enddo
!        all new coeffs are positive.  loop back to beginning.
go to 30

!                        ******  end of main loop  ******

!                        come to here for termination.
!                     compute the norm of the final residual vector.

350 sm = zero
if (npp1 <= m) then
  sm = sum( b(npp1:m)**2 )
else
  w(1:n) = zero
endif
rnorm = sqrt(sm)
return

contains

subroutine solve_triangular(zz)

!     the following block of code is used as an internal subroutine
!     to solve the triangular system, putting the solution in zz().

real (8), intent(in out) :: zz(:)

do l = 1,nsetp
  ip = nsetp+1-l
  if (l  /=  1) zz(1:ip) = zz(1:ip) - a(1:ip,jj)*zz(ip+1)
  jj = indx(ip)
  zz(ip) = zz(ip) / a(ip,jj)
enddo

return
end subroutine solve_triangular

end subroutine nnls



subroutine g1(a, b, cterm, sterm, sig)

!     compute orthogonal rotation matrix..

!  the original version of this code was developed by
!  charles l. lawson and richard j. hanson at jet propulsion laboratory
!  1973 jun 12, and published in the book
!  "solving least squares problems", prentice-hall, 1974.
!  revised feb 1995 to accompany reprinting of the book by siam.

!     compute.. matrix   (c, s) so that (c, s)(a) = (sqrt(a**2+b**2))
!                        (-s,c)         (-s,c)(b)   (   0          )
!     compute sig = sqrt(a**2+b**2)
!        sig is computed last to allow for the possibility that
!        sig may be in the same location as a or b .
!     ------------------------------------------------------------------

implicit none
real (8), intent(in)  :: a, b
real (8), intent(out) :: cterm, sterm, sig

!     local variables
real (8) :: one = 1.0d0, xr, yr, zero = 0.0d0
!     ------------------------------------------------------------------
if (abs(a) > abs(b)) then
  xr = b / a
  yr = sqrt(one + xr**2)
  cterm = sign(one/yr, a)
  sterm = cterm * xr
  sig = abs(a) * yr
  return
endif

if (b /= zero) then
  xr = a / b
  yr = sqrt(one + xr**2)
  sterm = sign(one/yr, b)
  cterm = sterm * xr
  sig = abs(b) * yr
  return
endif

!      sig = zero
cterm = zero
sterm = one
return
end subroutine g1



!     subroutine h12 (mode, lpivot, l1, m, u, up, c, ice, icv, ncv)

!  construction and/or application of a single
!  householder transformation..     q = i + u*(u**t)/b

!  the original version of this code was developed by
!  charles l. lawson and richard j. hanson at jet propulsion laboratory
!  1973 jun 12, and published in the book
!  "solving least squares problems", prentice-hall, 1974.
!  revised feb 1995 to accompany reprinting of the book by siam.
!     ------------------------------------------------------------------
!                     subroutine arguments

!     mode   = 1 or 2   selects algorithm h1 to construct and apply a
!            householder transformation, or algorithm h2 to apply a
!            previously constructed transformation.
!     lpivot is the index of the pivot element.
!     l1,m   if l1  <=  m   the transformation will be constructed to
!            zero elements indexed from l1 through m.   if l1 gt. m
!            the subroutine does an identity transformation.
!     u(),iue,up    on entry with mode = 1, u() contains the pivot
!            vector.  iue is the storage increment between elements.
!            on exit when mode = 1, u() and up contain quantities
!            defining the vector u of the householder transformation.
!            on entry with mode = 2, u() and up should contain
!            quantities previously computed with mode = 1.  these will
!            not be modified during the entry with mode = 2.
!     c()    on entry with mode = 1 or 2, c() contains a matrix which
!            will be regarded as a set of vectors to which the
!            householder transformation is to be applied.
!            on exit c() contains the set of transformed vectors.
!     ice    storage increment between elements of vectors in c().
!     icv    storage increment between vectors in c().
!     ncv    number of vectors in c() to be transformed. if ncv  <=  0
!            no operations will be done on c().
!     ------------------------------------------------------------------
subroutine h12(mode, lpivot, l1, m, u, up, c, ice, icv, ncv)
!     ------------------------------------------------------------------

implicit none
integer, intent(in)                     :: mode, lpivot, l1, m, ice, icv, ncv
real (8), dimension(:), intent(in out) :: u, c
real (8), intent(in out)               :: up

!  local variables
integer          :: i, i2, i3, i4, incr, j
real (8)        :: b, cl, clinv, one = 1.0d0, sm
!     ------------------------------------------------------------------
if (0 >= lpivot .or. lpivot >= l1 .or. l1 > m) return
cl = abs(u(lpivot))
if (mode /= 2) then
!                            ****** construct the transformation. ******
  do j = l1, m
    cl = max(abs(u(j)),cl)
  enddo
  if (cl <= 0) return
  clinv = one / cl
  sm = (u(lpivot)*clinv) ** 2 + sum( (u(l1:m)*clinv)**2 )
  cl = cl * sqrt(sm)
  if (u(lpivot) > 0) then
    cl = -cl
  endif
  up = u(lpivot) - cl
  u(lpivot) = cl
else
!            ****** apply the transformation  i+u*(u**t)/b  to c. ******

  if (cl <= 0) return
endif
if (ncv <= 0) return

b = up * u(lpivot)
!                       b  must be nonpositive here.  if b = 0., return.

if (b < 0) then
  b = one / b
  i2 = 1 - icv + ice * (lpivot-1)
  incr = ice * (l1-lpivot)
  do j = 1, ncv
    i2 = i2 + icv
    i3 = i2 + incr
    i4 = i3
    sm = c(i2) * up
    do i = l1, m
      sm = sm + c(i3) * u(i)
      i3 = i3 + ice
    enddo
    if (sm /= 0) then
      sm = sm * b
      c(i2) = c(i2) + sm * up
      do i = l1, m
        c(i4) = c(i4) + sm * u(i)
        i4 = i4 + ice
      enddo
    endif
  enddo ! j = 1, ncv
endif

return
end subroutine h12




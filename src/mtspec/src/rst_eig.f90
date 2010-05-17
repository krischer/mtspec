subroutine rst_eig(n,d1,d2,nev,ev,A)

!
!  Subroutine to get some eigenvalues of a real symmetric tridiagonal
!  matrix, using the LAPACK subroutines. Calls the LAPACK driver
!  routine dstevr.f 
! 
!  Input
!	n		The dimension of the matrix
!	d1(n)		The main diagonal of the matrix
!       d2(n-1)         The off-diagonal of the matrix
!	nev		The number of eigenvalues to get
!
!  Output
!	ev(nev)		Real vector with eigenvalues, the first one 
!			being the largest one.
!	A(n,nev)	Real array with the eigenvectors correspon
!			ding to the eigenvalues in ev. 
!
!  Modified
!
!	German Prieto
!	June 19 2009
!  	This is a simple driver subroutine to call the LAPACK 
!       routines instead of EISPACK
!
!
!  call dstevr
!

!********************************************************************

   implicit none

!  Input Output

   integer, intent(in) :: n, nev

   real(8), dimension(n),   intent(in) :: d1 
   real(8), dimension(n-1), intent(in) :: d2 

   real(8), dimension(nev), intent(out)   :: ev
   real(8), dimension(n,nev), intent(out) :: A

!  LAPACK Things

   integer, parameter :: nb = 64
   real(8) :: vl, vu, eps
   integer :: nfound, i
   real(8), dimension(n)     :: w 
   real(8), dimension(n,n)   :: z 

   integer, dimension(2*n)     :: isuppz
   real(8), dimension((nb+6)*n)     :: work
   integer :: lwork, liwork, info, il, iu
   integer, dimension(10*n)     :: iwork

!********************************************************************

   eps = 0.d0
   il = n - nev + 1
   iu = n

   lwork  = (nb+6)*n
   liwork = 10*n

   call dstevr('Vectors','I', n, d1, d2, vl, vu, il, iu, eps,      &
                        nfound, w, z, n, isuppz, work, lwork, iwork, &
                        liwork, info )
   if (info > 0) then
      write(6,'(a,i5)') 'dstevr error, illegal value, ', info
      stop
   elseif (info < 0) then
      write(6,'(a,i5)') 'dstevr internal error, ', info
      stop
   endif

   if (nfound /= nev) then
      write(6,'(a,i5)') 'dstevr error, eigenvalues found ', nfound
      stop
   endif

!  Reorganice eigenvalues and eigenfunctions to g0>g1>g2>...>gi
!  Also make positive standard (as dpss) subroutine

   do i = 1,nev

      ev(i)    = w(nev+1 - i)
      A(:,i)   = z(:,nev+1 - i)

      if (A(n/2 + 1,i) < 0.d0) then
         A(:,i) = -A(:,i)
      endif

   enddo

   return

end subroutine rst_eig


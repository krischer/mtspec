!--------------------------------------------------------------------

subroutine rsm_eig( n, K, nev, ev, A)

!
!  Subroutine to get some eigenvalues of a real symmetric square
!  matrix, using the LAPACK subroutines. Calls the LAPACK driver
!  routine dsyevr.f 
! 
!  Input
!	n		The dimension of the matrix
!	K(n,n)		The real symmetric matrix
!	nev		The number of eigenvalues to get
!
!  Output
!	ev(nev)		Real vector with eigenvalues, the first one 
!			being the largest one.
!	A(n,nev)	Real array with the eigenvectors correspon
!			ding to the eigenvalues in ev. 
!
!  Modified
!	German Prieto
!	Oct 20 2005
!
!	German Prieto
!	June 18 2009
!  	Changed to use the LAPACK routines instead of EISPACK
!
!
!  call dsyevr
!

!********************************************************************

   implicit none

!  Input Output

   integer, intent(in) :: n, nev

   real(8), dimension(n,n), intent(in) :: K

   real(8), dimension(nev), intent(out)   :: ev
   real(8), dimension(n,nev), intent(out) :: A
   
   
   real(8) :: eps, lb, ub
   integer :: i

!  LAPACK Things

   integer, parameter :: nb = 64
   real(8) :: vl, vu
   integer :: nfound
   real(8), dimension(n)     :: w 
   real(8), dimension(n,n)   :: v3

   integer, dimension(2*n)     :: isuppz
   real(8), dimension((nb+6)*n)     :: work
   integer :: lwork, liwork, info, il, iu
   integer, dimension(10*n)     :: iwork
 
!********************************************************************

   eps = 0.d0

   lwork  = (nb+6)*n
   liwork = 10*n
 
   il = n - nev + 1
   iu = n
 
   if (nev < n) then
      call dsyevr('Vectors','I','Upper',n,K,n,vl,vu,il,iu,eps,nfound, &
                    w,v3,n,isuppz,work,lwork,iwork,liwork,info)
   else
      call dsyevr('Vectors','A','Upper',n,K,n,vl,vu,il,iu,eps,nfound, &
                    w,v3,n,isuppz,work,lwork,iwork,liwork,info)
   endif

   if (nfound /= nev) then
      write(6,'(a,i5)') 'nsQI dsyevr error, eigenvalues found ', nfound
      stop
   endif

!  Reorganice eigenvalues and eigenfunctions to g0>g1>g2>...>gi
!  Also make positive standard (as dpss) subroutine

   do i = 1,nev

      ev(i)    = w(nev+1 - i)
      A(:,i)   = v3(:,nev+1 - i)

      if (A(n/2 + 1,i) < 0.d0) then
         A(:,i) = -A(:,i)
      endif

   enddo

   return

end subroutine rsm_eig

!--------------------------------------------------------------------


subroutine rsm_eig( n, K, nev, ev, A)

!
!  Subroutine to get some eigenvalues of a real symmetric square
!  matrix, using the EISPACK subroutines. Based on rsm from eispack.
! 
!  Input
!	n		The dimension of the matrix
!	K(n,n)		The real symmetric matrix
!	nev		The number of eigenvalues to get
!
!  Output
!	ev(nev)		Real vector with eigenvalues, the first one 
!			being the largest one.
!	A(n,nal)	Real array with the eigenvectors correspon
!			ding to the eigenvalues in ev. 
!
!  Modified
!	German Prieto
!	Oct 20 2005
!
!  call tred1, tridib, tinvit, trbak
!

!********************************************************************

   implicit none

!  Input Output

   integer, intent(in) :: n, nev

   real(8), dimension(n,n), intent(in) :: K

   real(8), dimension(nev), intent(out)   :: ev
   real(8), dimension(n,nev), intent(out) :: A
   
   
   real(8) :: eps, lb, ub
   integer :: m11, ierr, i

   real(8), dimension(n) :: fv1, fv2, fv3

   real(8), dimension(n,nev) :: v2
   real(8), dimension(nev)   :: ev2
   real(8), dimension(nev)   :: ind
 
!********************************************************************

   eps = 0.d0
   m11 = (n) - (nev-1)
    
!  Make tridiagonal matrix
  
   call tred1  ( n, K, fv1, fv2, fv3 )

!  Get the largest nbl eigenvalues
   
   call tridib ( n,eps,fv1,fv2,fv3,lb,ub,m11,nev,ev2,ind,ierr )
   if (ierr .ne. 0) then
      write(6,'(a,i5)') 'nsQI tridib error ',ierr
      stop
   endif

!  Get the nbl eigenfunctions
   
   call tinvit ( n,fv1,fv2,fv3,nev,ev2,ind,v2,ierr )
   if (ierr .ne. 0) then
      write(6,'(a,i5)') 'nsQI tinvit error ',ierr
      stop
   endif

!  Transform back the eigenfunctions (inverse tred1 function)

   call trbak1 ( n,K,fv2,nev,v2 )

!  Reorganice eigenvalues and eigenfunctions to g0>g1>g2>...>gi
!  Also make positive standard (as dpss) subroutine

   do i = 1,nev

      ev(i)    = ev2(nev+1 - i)
      A(:,i)   = v2(:,nev+1 - i)

      if (A(n/2 + 1,i) < 0.d0) then
         A(:,i) = -A(:,i)
      endif

   enddo


end subroutine rsm_eig

!--------------------------------------------------------------------




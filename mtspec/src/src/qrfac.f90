subroutine qrfac(m, n, a_in, q, r)

!
!  QR decomposition of A where A is an  m by n  matrix
!  Method - modified Gram-Schmidt.
!  See Golub and Van Loan: Matrix Computations (p 152)
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in) :: m, n
   real(4), dimension(m,n), intent(in) :: a_in

!  Output

   real(4), dimension(m,n), intent(out) :: q
   real(4), dimension(n,n), intent(out) :: r

!  Working variables

   real(4), dimension(m,n) :: a
   integer :: i, j, k
   
!********************************************************************

   a(:,:) = a_in(:,:)		! Avoid overwriting the input matrix

   do k=1,n

!  find constants for rotation and diagonal entry

      r(k,k)=0.00
      do i=1,m
         r(k,k)=a(i,k)*a(i,k) + r(k,k)
      enddo
      r(k,k)=sqrt(r(k,k))

      do i=1,m
         a(i,k) = a(i,k)/r(k,k)
      enddo
      do j=k+1,n
         r(k,j) = 0.0
         do i=1,m
            r(k,j) = a(i,k)*a(i,j) + r(k,j)
         enddo
         do i=1,m
            a(i,j) = a(i,j) - a(i,k)*r(k,j)
         enddo
      enddo
   enddo

   q = a

   return

end subroutine qrfac

!--------------------------------------------------------------------






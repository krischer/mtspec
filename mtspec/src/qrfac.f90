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

subroutine qrfac8(m, n, a_in, q, r)

!
!  QR decomposition of A where A is an  m by n  matrix
!  Method - modified Gram-Schmidt.
!  See Golub and Van Loan: Matrix Computations (p 152)
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in) :: m, n
   real(8), dimension(m,n), intent(in) :: a_in

!  Output

   real(8), dimension(m,n), intent(out) :: q
   real(8), dimension(n,n), intent(out) :: r

!  Working variables

   real(8), dimension(m,n) :: a
   integer :: i, j, k
   
!********************************************************************

   a(:,:) = a_in(:,:)		! Avoid overwriting the input matrix

   do k=1,n

!  find constants for rotation and diagonal entry

      r(k,k)=0.0d0
      do i=1,m
         r(k,k)=a(i,k)*a(i,k) + r(k,k)
      enddo
      r(k,k)=sqrt(r(k,k))

      do i=1,m
         a(i,k) = a(i,k)/r(k,k)
      enddo
      do j=k+1,n
         r(k,j) = 0.d0
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

end subroutine qrfac8

!--------------------------------------------------------------------

subroutine qrcov(m, n,a_in, cov)

!********************************************************************

   implicit none

!  Input

   integer, intent(in)    :: m, n
 
   real(8), dimension(m,n), intent(in) :: a_in

!  Output

   real(8), dimension(n,n) :: cov

   real(8), dimension(m,n) :: a

!  Other

   real(8)                 :: s

   integer :: l, l1, i, j, j1, n1

!********************************************************************

   if (m < n) then
      return
   endif

   a(:,:) = a_in(:,:)

   do i = 1,n
      a(i,i) = 1.d0/a(i,i)
   enddo

   if (n > 1) then     
      n1 = n - 1 
      do i = 1,n1    
         j1 = i + 1 
         do j = j1,n    
           s = 0.d0     
           l1 = j - 1 
           do l = i,l1    
              s = s + a(i,l)*a(l,j)    
           enddo
           a(i,j) = -s*a(j,j)
         enddo
      enddo  
   endif


   do i = 1,n     
      do j = 1,n     
         s = 0.d0     
         do l = j,n     
            s = s + a(i,l)*a(j,l)    
         enddo
         a(i,j) = s 
      enddo 
   enddo    

   cov = a(1:n,1:n)

   return

end subroutine qrcov







   subroutine zqrfac4(m, n,a_in, q, r)

!$$$$  calls no other routines
!  Forms a QR decomposition of a = q*r
!  Method - modified Gram-Schmidt.
!  See Golub and Van Loan: Matrix Computations (p 152)
!  m, n dimension of a 
!  a    matrix ---- overwritten with q array with n orthogonal cols
!  r    upper triangle such that a = q*r
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in) :: m, n
   complex(4), dimension(m,n), intent(in) :: a_in

!  Output

   complex(4), dimension(m,n), intent(out) :: q 
   complex(4), dimension(n,n), intent(out) :: r

!  Working variables

   integer :: i, k, j
   complex(4), dimension(m,n) :: a

!********************************************************************

   a(:,:) = a_in(:,:)		! Avoid overwriting the input matrix

   do k=1,n

!  find constants for rotation and diagonal entry

      r(k,k)=0.00
      do i=1,m
         r(k,k)=conjg(a(i,k))* a(i,k) + r(k,k)
      enddo
      r(k,k)=sqrt(r(k,k))

      do i=1,m
         a(i,k) = a(i,k)/r(k,k)
      enddo
      do j=k+1,n
         r(k,j) = 0.0
         do i=1,m
            r(k,j) =conjg(a(i,k))*a(i,j) + r(k,j)
         enddo
         do i=1,m
            a(i,j) = a(i,j) - a(i,k)*r(k,j)
         enddo
      enddo
   enddo
   
   q = a

   return
      
end subroutine zqrfac4 

!--------------------------------------------------------------------

   subroutine zqrfac(m, n,a_in, q, r)

!$$$$  calls no other routines
!  Forms a QR decomposition of a = q*r
!  Method - modified Gram-Schmidt.
!  See Golub and Van Loan: Matrix Computations (p 152)
!  m, n dimension of a 
!  a    matrix ---- overwritten with q array with n orthogonal cols
!  r    upper triangle such that a = q*r
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in) :: m, n
   complex(8), dimension(m,n), intent(in) :: a_in

!  Output

   complex(8), dimension(m,n), intent(out) :: q 
   complex(8), dimension(n,n), intent(out) :: r

!  Working variables

   integer :: i, k, j
   complex(8), dimension(m,n) :: a

!********************************************************************

   a(:,:) = a_in(:,:)		! Avoid overwriting the input matrix

   do k=1,n

!  find constants for rotation and diagonal entry

      r(k,k)=0.d0
      do i=1,m
         r(k,k)=conjg(a(i,k))* a(i,k) + r(k,k)
      enddo
      r(k,k)=sqrt(r(k,k))

      do i=1,m
         a(i,k) = a(i,k)/r(k,k)
      enddo
      do j=k+1,n
         r(k,j) = 0.d0
         do i=1,m
            r(k,j) =conjg(a(i,k))*a(i,j) + r(k,j)
         enddo
         do i=1,m
            a(i,j) = a(i,j) - a(i,k)*r(k,j)
         enddo
      enddo
   enddo
   
   q = a

   return
      
end subroutine zqrfac 

!--------------------------------------------------------------------
! The Covariance matrix using the QR decomposition
!--------------------------------------------------------------------

subroutine zqrcov(m, n,a_in, cov)

!********************************************************************

   implicit none

!  Input

   integer, intent(in)    :: m, n
 
   complex(8), dimension(m,n), intent(in) :: a_in

!  Output

   real(8), dimension(n,n), intent(out) :: cov

   complex(8), dimension(m,n) :: a
   complex(8)                 :: s

   integer :: l, l1, i, j, j1, n1

!********************************************************************

   if (m < n) then
      return
   endif

   a(:,:) = a_in(:,:)

   do i = 1,n
      a(i,i) = 1.d0/(a(i,i))
   enddo

   if (n > 1) then     
      n1 = n - 1 
      do i = 1,n1    
         j1 = i + 1 
         do j = j1,n    
           s = 0.d0     
           l1 = j - 1 
           do l = i,l1    
              s = s + conjg(a(i,l)) * a(l,j)    
           enddo
           a(i,j) = -s*a(j,j)
         enddo
      enddo  
   endif


   do i = 1,n     
      do j = 1,n     
         s = 0.d0     
         do l = j,n     
            s = s + conjg(a(i,l)) * a(j,l)    
         enddo
         a(i,j) = s 
      enddo 
   enddo    

!  Fill out to make symmetric

   do j = 1,n
      do i = 1,j
         a(j,i) = a(i,j)
      enddo
   enddo

   cov = dble(a(1:n,1:n))
 
   return

end subroutine zqrcov

!--------------------------------------------------------------------



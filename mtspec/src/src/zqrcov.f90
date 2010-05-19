subroutine zqrcov(m, n,a_in, cov)

!********************************************************************

   implicit none

!  Input

   integer, intent(in)    :: m, n
 
   complex(8), dimension(m,n), intent(in) :: a_in

!  Output

   real(8), dimension(n,n) :: cov

   complex(8), dimension(m,n) :: a

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
           s = 0.     
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
         s = 0.     
         do l = j,n     
            s = s + a(i,l)*a(j,l)    
         enddo
         a(i,j) = s 
      enddo 
   enddo    

   cov = a(1:n,1:n)

   return

end subroutine zqrcov

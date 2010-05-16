subroutine sft(x,n,om,ct,st)

!
!  Modified
!	German Prieto
!	November 2004
!
!   calculates fourier transform of real sequence x(i),i=1,...n
!   at angular frequency om normalized so that nyquist=pi. the sine
!   transform is returned in st and the cosine transform in ct.
!   algorithm is that of goertzal with modifications by
!   gentleman, comp.j. 1969
!   transform is not normalized
!   to normalize one-sided ft, divide by sqrt(data length)
!   for positive om, the ft is defined as ct-(0.,1.)st or like slatec
!   cfftf
! 

!**********************************************************************

   implicit none

   real(8), parameter :: pi=3.141592653589793238d0, tp=2.d0*pi

   integer :: k, n, np1, l

   real(8) :: b, e, d, c, a, s, om, ct, st 
 
   real(8), dimension(n) :: x

!**********************************************************************

   np1=n+1
   l=6.d0*om/tp
   s=sin(om)
   a=0.d0
   c=0.d0
   d=0.d0
   e=0.d0
   
   if (l.eq.0)then

!     recursion for low frequencies (.lt. nyq/3)

      b=-4.d0*sin(om/2.d0)**2
      do k=1,n
         c=a
         d=e
         a=x(np1-k)+b*d+c
         e=a+d
      enddo
      
   elseif (l.eq.1)then

!     regular goertzal algorithm for intermediate frequencies

      b=2.d0*cos(om)
      do  k=1,n
          a=x(np1-k)+b*e-d
          d=e
          e=a
      enddo
      
   else

!     recursion for high frequencies (.gt. 2*nyq/3)
      
      b=4.d0*cos(om/2.d0)**2
        do k=1,n
           c=a
           d=e
           a=x(np1-k)+b*d-c
           e=a-d
	enddo
   
   endif
   
   st=-s*d
   ct=a-b*d/2.d0
      
   return
      
end subroutine sft



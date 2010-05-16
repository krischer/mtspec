subroutine cumtrapz(npts,x,intx)

!
! Integrate vector in data using a trapezoidal rule. It is assumed 
! that dx = 1, one needs to multiply intx by the actual sampling to 
! obtain actual units. 
!
! INPUT
!          npts     number of points in data series x, 
!                   and output intx
!          x        real(8) vector with npts points
! OUTPUT
!          intx     the output, integrated x
!

!********************************************************************

   implicit none

   integer, intent(in)                   :: npts
   real(8), dimension(npts), intent(in)  :: x

   real(8), dimension(npts), intent(out) :: intx 

!  Other variables

   real(8) :: xsave1, xsave2
   integer :: i

!********************************************************************

!  Calculate first two values directly

   intx(1) = x(1)
   intx(2) = x(1) + x(2)

!  Do trapezoidal for 3 values

   xsave1  = x(1)
   xsave2  = x(3)
   intx(3) = 0.5d0*xsave1 + x(2) + 0.5d0*xsave2 
   
   do i = 4,npts
         intx(i) = intx(i-1) + 0.5d0*(xsave2+x(i))
         xsave2  = x(i)
   enddo

   return

end subroutine cumtrapz

!--------------------------------------------------------------------
! Non-negative integration
!--------------------------------------------------------------------

subroutine cumtrapz_nn(npts,x,intx)

!
! Integrate vector in data using a trapezoidal rule. It is assumed 
! that dx = 1, one needs to multiply intx by the actual sampling to 
! obtain actual units. 
!
! INPUT
!          npts     number of points in data series x, 
!                   and output intx
!          x        real(8) vector with npts points
! OUTPUT
!          intx     the output, integrated x
!

!********************************************************************

   implicit none

   integer, intent(in)                   :: npts
   real(8), dimension(npts), intent(in)  :: x

   real(8), dimension(npts), intent(out) :: intx 

!  Other variables

   real(8), dimension(npts)  :: x2
   real(8) :: xsave1, xsave2
   integer :: i

!********************************************************************

!  Calculate first two values directly

   intx(1) = x(1)
   intx(1) = max(intx(1),0.d0)
   intx(2) = x(1) + x(2)
   intx(2) = max(intx(2),0.d0)

!  Do trapezoidal for 3 values

   xsave1  = x(1)
   xsave2  = x(3)
   intx(3) = 0.5d0*xsave1 + x(2) + 0.5d0*xsave2 
   intx(3) = max(intx(3),0.d0)

   do i = 4,npts
         intx(i) = intx(i-1) + 0.5d0*(xsave2+x(i))
         intx(i) = max(intx(i),0.d0)
         xsave2  = x(i)
   enddo

   return

end subroutine cumtrapz_nn




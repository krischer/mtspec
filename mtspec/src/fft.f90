subroutine fft_r(data,npts,fx)

!
!  Modified
!	German Prieto
!	July 2009
!
!  The discrete fourier tranform of a real data set of size npts
!
!  Input/Output 	
!
!	data	the real data to be transformed, data(npts)
!
!  Input
!
!	npts	the length of the series data
!
!  calls fftw routines
!

!**********************************************************************

   implicit none

   integer, intent(in) :: npts
   
   real(4),    intent(in),  dimension(npts) :: data
   complex(4), intent(out), dimension(npts) :: fx
  
!  FFTW

   complex(8),  dimension(npts) :: x, y

   integer(8) :: plan

   integer, parameter :: i=-1, j=64
 
!**********************************************************************

   x = cmplx(data,kind=8)

   call dfftw_plan_dft_1d(plan,npts,x,y,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   fx = cmplx(y,kind=4)

   return

end subroutine fft_r

!======================================================================

subroutine fft_d(data,npts,fx)

!
!  Modified
!	German Prieto
!	July 2009
!
!  The discrete fourier tranform of a real data set of size npts
!
!  Input/Output 	
!
!	data	the real data to be transformed, data(npts)
!
!  Input
!
!	npts	the length of the series data
!
!  calls fftw routines
!

!**********************************************************************

   implicit none

   integer, intent(in) :: npts
   
   real(8),    intent(in),  dimension(npts) :: data
   complex(8), intent(out), dimension(npts) :: fx
  
!  FFTW

   complex(8),  dimension(npts) :: x

   integer(8) :: plan

   integer, parameter :: i=-1, j=64
 
!**********************************************************************

   x = cmplx(data)

   call dfftw_plan_dft_1d(plan,npts,x,fx,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   return

end subroutine fft_d

!======================================================================


subroutine fft_c(data,npts,fx)

!
!  Modified
!	German Prieto
!	July 2009
!
!  The discrete fourier tranform of a real data set of size npts
!
!  Input/Output 	
!
!	data	the real data to be transformed, data(npts)
!
!  Input
!
!	npts	the length of the series data
!
!  calls fftw routines
!

!**********************************************************************

   implicit none

   integer, intent(in) :: npts
   
   complex(4), intent(in),  dimension(npts) :: data
   complex(4), intent(out), dimension(npts) :: fx
  
!  FFTW

   complex(8), dimension(npts) :: x, y

   integer(8) :: plan

   integer, parameter :: i=-1, j=64
 
!**********************************************************************

   x = cmplx(data,kind=8)

   call dfftw_plan_dft_1d(plan,npts,data,y,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   fx = cmplx(y,kind=4)

   return

end subroutine fft_c

!======================================================================

subroutine fft_z(data,npts,fx)

!
!  Modified
!	German Prieto
!	July 2009
!
!  The discrete fourier tranform of a real data set of size npts
!
!  Input/Output 	
!
!	data	the real data to be transformed, data(npts)
!
!  Input
!
!	npts	the length of the series data
!
!  calls fftw routines
!

!**********************************************************************

   implicit none

   integer, intent(in) :: npts
   
   complex(8), intent(in),  dimension(npts) :: data
   complex(8), intent(out), dimension(npts) :: fx
  
!  FFTW

   integer(8) :: plan

   integer, parameter :: i=-1, j=64
 
!**********************************************************************

   call dfftw_plan_dft_1d(plan,npts,data,fx,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   return

end subroutine fft_z

!======================================================================



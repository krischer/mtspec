subroutine ifft_r(data,npts,tx)

!
!  Modified
!	German Prieto
!	July 2009
!
!  The discrete fourier tranform of a real data set of size npts
!
!  Input/Output 	
!
!	data	the complex freq domain data to be transformed, data(npts)
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
   real(4),    intent(out), dimension(npts) :: tx
  
!  FFTW

   complex(8),  dimension(npts) :: x, y

   integer(8) :: plan

   integer, parameter :: i=1, j=64
 
!**********************************************************************

   x = cmplx(data,kind=8)

   call dfftw_plan_dft_1d(plan,npts,x,y,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   tx = real(y)

   return

end subroutine ifft_r

!======================================================================

subroutine ifft_d(data,npts,tx)

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
   real(8),    intent(out), dimension(npts) :: tx
  
!  FFTW

   complex(8),  dimension(npts) :: y

   integer(8) :: plan

   integer, parameter :: i=1, j=64
 
!**********************************************************************

   call dfftw_plan_dft_1d(plan,npts,data,y,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   tx = dble(y)

   return

end subroutine ifft_d

!======================================================================


subroutine ifft_c(data,npts,tx)

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
   complex(4), intent(out), dimension(npts) :: tx
  
!  FFTW

   complex(8), dimension(npts) :: x, y

   integer(8) :: plan

   integer, parameter :: i=1, j=64
 
!**********************************************************************

   x = cmplx(data,kind=8)

   call dfftw_plan_dft_1d(plan,npts,x,y,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   tx = cmplx(y,kind=4)

   return

end subroutine ifft_c

!======================================================================

subroutine ifft_z(data,npts,tx)

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
   complex(8), intent(out), dimension(npts) :: tx
  
!  FFTW

   integer(8) :: plan

   integer, parameter :: i=1, j=64
 
!**********************************************************************

   call dfftw_plan_dft_1d(plan,npts,data,tx,  &
                  i,j)
   call dfftw_execute(plan)
   call dfftw_destroy_plan(plan)

   return

end subroutine ifft_z

!======================================================================




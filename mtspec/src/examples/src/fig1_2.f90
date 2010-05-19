program fig1_2

!
! Simple code to generate Figure 1 and 2 of
! Prieto, G. A., R. L. Parker, and F. L. Vernon (2008) 
! A Fortran 90 library formultitaper spectrum analysis
! 
! Additional editing of the figure was performed for publication.
! An additional on-the-fly plotting library is used for the 
! plotting of the data, available at
! pangea.stanford.edu/~gprieto/software.html
!
! Written by
!	G. A. Prieto
!	January 2nd, 2008
!
! Comments, questions, bugs? 
! Please email gprieto@stanford.edu 
!
! Calls: mtspec, gplot
! Modules: spectra.mod, plot.mod


!********************************************************************

   use spectra 
   use plot

   implicit none

   integer, parameter :: npts=156, nfft=312, nf = 312/2+1

   integer :: kspec, i, rshape

   real(4) :: tbnw, dt, fcrit

   real(4), dimension(npts) :: data, t

   real(4), dimension(nf)     :: freq, spec, spec2, fstat
   real(4), dimension(nf,2)   :: err


!********************************************************************

   kspec  = 5
   tbnw   = 3.5
   dt     = 4930.
  
!  Load the data
 
   open(12,file='../data/v22_174_series.dat')

   do i = 1,npts
      read(12,*) data(i)
      t(i) = real(i)*dt
   enddo

   close(12)

!  Plot time series

   call gplot(t,data)

!  Compute simple MT spectrum

   call mtspec(npts,nfft,dt,data,tbnw,kspec,nf,freq,spec,err=err,    &
                           verb='y')

   freq = freq * 1.e6	! Nicer units

   call gplot(freq,spec,'hold',logxy='linlog',ylimit='5 1e0 1e6')
   call gplot(freq,err,dash='--',logxy='linlog',ylimit='5 1e0 1e6')

!  Compute spectrum with lines, with zero padding of length nfft.
!	nfft = 2*npts   in this case.

   rshape = 0 
   fcrit  = 0.9

   call mtspec(npts,nfft,dt,data,tbnw,kspec,nf,freq,spec2,    &
                           rshape=rshape,fcrit=fcrit,fstat=fstat,     &
                           verb='y')

   freq = freq * 1.e6	! Nicer units

!  Plot reshaped spectrum

   call gplot(freq,spec2,logxy='linlog',ylimit='5 1e0 1e6' )

!  Plot F-statistics

   call gplot(freq,fstat)

end program fig1_2



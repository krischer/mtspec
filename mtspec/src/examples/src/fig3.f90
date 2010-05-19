program fig3

!
! Simple code to generate Figure 3 of
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
! Calls: mtspec, sine_psd, gplot
! Modules: spectra.mod, plot.mod


!********************************************************************

   use spectra 
   use plot

   implicit none

   integer, parameter :: npts=86400, nf = 86400/2+1

   integer :: kspec, i, qispec, ntap, ntimes

   real(4) :: tbnw, dt, fact

   real(4), dimension(npts) :: data, t

   real(4), dimension(nf)     :: freq, spec, spec2, spec3

   integer :: count1, count2, count_rate

!********************************************************************

   dt     = 1.
  
!  Load the data
 
   open(12,file='../data/PASC.dat')

   do i = 1,npts
      read(12,*) data(i)
      t(i) = real(i)*dt
   enddo

   close(12)

!  The time series is very long, not plotted here.

!  Compute simple MT spectrum

   kspec  = 5
   tbnw   = 4.5

   call system_clock(count1, count_rate)

   call mtspec(npts,dt,data,tbnw,kspec,nf,freq,spec)

   call system_clock(count2, count_rate)

   print *, 'Elapsed time ', real(count2-count1)/real(count_rate)

   call gplot(freq(2:nf),spec(2:nf),logxy='loglog',ylimit='5 1e0 1e9')

!  Compute single taper spectrum

   kspec  = 1
   tbnw   = 1.5

   call mtspec(npts,dt,data,tbnw,kspec,nf,freq,spec)

   call gplot(freq(2:nf),spec(2:nf),logxy='loglog',ylimit='5 1e0 1e9')

!  Compute quadratic spectrum

   kspec  = 5
   tbnw   = 4.5
   qispec = 1

   call system_clock(count1, count_rate)

   call mtspec(npts,dt,data,tbnw,kspec,nf,freq,spec2,qispec=qispec)

   call system_clock(count2, count_rate)
   print *, 'Elapsed time ', real(count2-count1)/real(count_rate)

   call gplot(freq(2:nf),spec2(2:nf),logxy='loglog',ylimit='5 1e0 1e9')

!  Compute the sine multitaper spectrum

   ntap = 0	! Allow adaptive method
   ntimes = 2	! Iterate two times
   fact = 1.0	! degree of smoothing

   call system_clock(count1, count_rate)

   call sine_psd(npts,dt,data,ntap,ntimes,fact,nf,freq,spec3)

   call system_clock(count2, count_rate)

   print *, 'Elapsed time ', real(count2-count1)/real(count_rate)

   call gplot(freq(2:nf),spec3(2:nf),logxy='loglog',ylimit='5 1e0 1e9')

end program fig3



program fig5

!
! Simple code to generate Figure 5 of
! Prieto et al. 
! A Fortran 90 library formultitaper spectrumanalysis
! 
! Additional editing of the figure was performed for 
! publication.
! An additional on-the-fly plotting library is used
! for the plotting of the data. 
!

!********************************************************************

   use spectra 
   use plot

   implicit none

   integer, parameter :: npts=86400, nfft = 2*86400, nf = 2*86400/2+1
   integer, parameter :: ngf = 500, nf2 = ngf/2+1

   integer :: kspec, i, iadapt

   real(4) :: tbnw, dt

   real(4),    dimension(npts)     :: pasc, ado, t

   complex(4), dimension(nfft)     :: trf, pasc_cmp, ado_cmp, cc

   real(4), dimension(ngf)         :: gf, gf2

   real(4), dimension(nf2)         :: freq, spec


!********************************************************************

   dt     = 1.
   kspec  = 7
   tbnw   = 4.
   iadapt = 0		! Adaptive multitaper

!  Load the data, already resampled
 
   open(12,file='../data/PASC_jan04_2007.dat')

   do i = 1,npts
      read(12,*) pasc(i)
      t(i) = real(i)*dt
   enddo

   close(12)

   open(12,file='../data/ADO_jan04_2007.dat')

   do i = 1,npts
      read(12,*) ado(i)
   enddo

   close(12)

   pasc = pasc - sum(pasc)/real(npts)
   ado  = ado  - sum(ado) /real(npts)

!  Call transfer function subroutine

   call mt_transfer (npts,nfft,dt,pasc,ado,tbnw,kspec,nf,   &
                   freq=freq,trf=trf,iadapt=iadapt,demean=1)  

   call ifft4(trf,nfft) 

   do i = 1,ngf
      gf(i) = real(trf(nfft-i+1))
   enddo

   call gplot(t(1:ngf),gf,ylimit = '3',xlimit='6',output='gf1.ps')

!  The correlation approach

   pasc_cmp         = 0.
   pasc_cmp(1:npts) = pasc
   ado_cmp          = 0.
   ado_cmp(1:npts)  = ado

   call fft4(pasc_cmp,nfft)
   call fft4(ado_cmp,nfft)

   cc = pasc_cmp * conjg(ado_cmp)

   call ifft4(cc,nfft)

   do i = 1,ngf
      gf2(i) = real(cc(nfft-i+1))
   enddo

   call gplot(t(1:ngf),gf2,ylimit = '3',xlimit='6',output='gf2.ps')

!  Single taper Spectral estimates

   tbnw  = 1.5
   kspec = 1

   call mtspec(ngf,dt,gf,tbnw,kspec,nf2,freq,spec)

   call gplot(freq,spec,'hold',logxy='loglog')

   call mtspec(ngf,dt,gf2,tbnw,kspec,nf2,freq,spec)

   call gplot(freq,spec,logxy='loglog',output='gfspec.ps')

end program fig5



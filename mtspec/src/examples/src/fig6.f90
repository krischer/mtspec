program fig6

!
! Simple code to generate Figure 6 of
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
! Calls: mt_deconv, ifft4, fft4, mtspec, gplot
! Modules: spectra.mod, mvspectra.mod, plot.mod
! 	


!********************************************************************

   use spectra 
   use mvspectra
   use plot

   implicit none

   integer, parameter :: npts=86400, nfft = 2*86400, nf = 2*86400/2+1
   integer, parameter :: ngf = 500, nf2 = ngf/2+1

   integer :: kspec, i, iadapt

   real(4) :: tbnw, dt

   real(4),    dimension(npts)     :: pasc, ado, t

   complex(4), dimension(nfft)     :: pasc_cmp, ado_cmp, cc

   real(4), dimension(nfft)        :: tgf

   real(4), dimension(ngf)         :: gf, gf2

   real(4), dimension(nf2)         :: freq, spec


!********************************************************************

   dt     = 1.
   kspec  = 7
   tbnw   = 4.
   iadapt = 0		! Adaptive multitaper

!  Load the data, already resampled
 
   open(12,file='../data/PASC.dat')

   do i = 1,npts
      read(12,*) pasc(i)
      t(i) = real(i)*dt
   enddo

   close(12)

   open(12,file='../data/ADO.dat')

   do i = 1,npts
      read(12,*) ado(i)
   enddo

   close(12)

   pasc = pasc - sum(pasc)/real(npts)
   ado  = ado  - sum(ado) /real(npts)

!  Call deconvolution subroutine

   call mt_deconv ( npts,nfft,dt,pasc,ado,tbnw,kspec,nf,    &
                     tfun=tgf,iadapt=iadapt, demean=1 ) 

   do i = 1,ngf
      gf(i) = real(tgf(nfft-i+1))
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

!  Single taper spectral estimates

   tbnw  = 1.5
   kspec = 1

   call mtspec(ngf,dt,gf,tbnw,kspec,nf2,freq,spec)

   call gplot(freq,spec,'hold',logxy='loglog')

   call mtspec(ngf,dt,gf2,tbnw,kspec,nf2,freq,spec)

   call gplot(freq,spec,logxy='loglog',output='gfspec.ps')

end program fig6

!-



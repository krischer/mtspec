program fig4_5

!
! Simple code to generate Figure 4 and 5 of
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
! Calls: mt_transfer, gplot
! Modules: mvspectra.mod, plot.mod
! 	

!********************************************************************

   use mvspectra 
   use plot

   implicit none

   integer, parameter :: npts=4458, nfft = 4*8916, nf = 4*8916/2+1

   integer :: kspec, i, iadapt

   real(4) :: tbnw, dt, junk

   real(4),    dimension(npts)     :: ext1, int1, t

   real(4),    dimension(nf)       :: freq, spec, cohe, wt

   complex(4), dimension(nfft)     :: trf

   complex(4), dimension(nf-1)     :: Qi

   real(4), dimension(nf-1)        :: per, lper

!  Band averging

   real(4), dimension(10)          :: avper, crvar, civar
   complex(4), dimension(10)       :: c, cavg, travg

   real(4)               :: swt, l
   complex(4)            :: Q2c
   integer               :: fcnt, j, iloc1, iloc2
   integer, dimension(1) :: i1, i2 

!********************************************************************

   dt     = 3600.
   kspec  = 12
   tbnw   = 7.5
   iadapt = 1		! Adaptive multitaper

!  Load the data, already resampled
 
   open(12,file='../data/asc_akima.dat')

   do i = 1,npts
      read(12,*) junk, ext1(i), int1(i)
      t(i) = real(i)*dt
   enddo

   close(12)

!  Plot time series

   call gplot(t/1.e6,int1,ylimit='5 -120 20')
   call gplot(t/1.e6,ext1,ylimit='5 -120 20')

!  Demean the two series (maybe not needed, result does not change)

   int1 = int1 - sum(int1)/real(npts)
   ext1 = ext1 - sum(ext1)/real(npts)

!  Call transfer function subroutine

   call mt_transfer (npts,nfft,dt,int1,ext1,tbnw,kspec,nf,   &
                   freq=freq,cohe=cohe,trf=trf,iadapt=iadapt)  

   call gplot(freq*86400.,cohe)	! cycles per day

!  Compute Qi

   do i = 2,nf

      if (cohe(i) >= 0.6) then
         wt(i) = 1./sqrt(1. - cohe(i))
      else
         wt(i) = 0.
      endif

      Qi(i) = trf(i) 

   enddo

!  Band averaging (same periods as Constable and Constable (2004)

   per = 1./freq(2:nf)

   lper = log10(per)

   avper(1)  = 21330. 
   avper(2)  = 41410. 
   avper(3)  = 74400. 
   avper(4)  = 185100. 
   avper(5)  = 348000.
   avper(6)  = 697800.
   avper(7)  = 1428000.
   avper(8)  = 2674000. 
   avper(9)  = 4593000. 
   avper(10) = 11810000.

   avper = log10(avper)

   cavg = 0.
   do i = 1,10

      fcnt = count(lper<=avper(i)+0.1 .and. lper>=avper(i)-0.1)

      if (fcnt > 1) then

         i1   = minloc(lper, lper >= avper(i)-0.1)
         i2   = maxloc(lper, lper <= avper(i)+0.1)

         iloc2 = i1(1)
         iloc1 = i2(1)

!  Weighted mean

         swt = 0.
         do j = 0,fcnt-1
            travg(i)  = travg(i) + wt(iloc1+j)*Qi(iloc1+j)
            swt  = swt + wt(iloc1+j)
         enddo
         travg(i) = travg(i)/swt

      elseif (fcnt == 1) then

         travg(i) = Qi(iloc1)  
      
      endif

   enddo

   cavg = 6378. * (1. - 2.*travg) / (2.*(1.+travg)) 

   call gplot(avper,real(cavg),'hold',xlimit='5 4. 7.5')
   call gplot(avper,imag(cavg),xlimit='5 4. 7.5',ylimit='4 -750 1500')

end program fig4_5



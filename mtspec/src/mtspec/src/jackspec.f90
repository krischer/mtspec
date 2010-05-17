subroutine jackspec(nfft,ne,kspec,evalu1,yk,	&
                    spec,se,wt,err)

!
!	
!  subroutine to calculate adaptively weighted jackknifed 95% 
!  confidence limits
!
!  inputs:
!
!    nfft  - number of points in fft's
!    ne    - number of elements in fft to process, normally nfft/2+1
!            for real data and nfft for complex data
!    kspec - number of prolate windows used
!    evalu1- double precision vector containing 1 minus eigenvalues
!    yk    - complex array containing kspec ffts of length nfft
!    spec - real vector containing adaptively weighted spectrum
!    se   - real vector containing the number of degrees of freedom
!           for the spectral estimate at each frequency.
!    wt   - real array containing the ne weights for kspec 
!           eigenspectra normalized so that if there is no bias, the
!           weights are unity.
!   

!
!  outputs:
!
!    err  - real array of jackknife error estimates, with 5 and 95%
!           confidence intervals of the spectrum.
!
!
!  calls qtdis
!
!  Modified
!	German Prieto
!	Aug 2006
!	
!	German Prieto
!	March 2007
!	Changed the Jackknife to be more efficient.
!


!**********************************************************************

   implicit none

!  Input Output

   integer,    intent(in)                         :: nfft, ne, kspec
   real(8),    intent(in),  dimension(kspec)      :: evalu1
   complex(8), intent(in),  dimension(nfft,kspec) ::  yk
   real(8),    intent(in),  dimension(ne)         :: spec, se 
   real(8),    intent(in),  dimension(ne,kspec)   :: wt

   real(8),    intent(out), dimension(ne,2)       :: err

!  Jackknife

   real(4) :: qtdis
   real(8), dimension(ne)  :: var
   real(8), dimension(ne)  :: lspec, lsjk_mean, bjk
   real(8), dimension(ne,kspec) :: sk, sjk, varjk
   real(8), dimension(ne,kspec) :: lsjk
   real(8), dimension(ne,kspec-1) :: sj, wjk

!  Other variables

   integer :: i, k, ks, nfft2
   real(8) :: rerr, df, dvar
   real(8), dimension(kspec) :: bk, varsk
   real(8), dimension(kspec) :: evalu, sqev 

!**********************************************************************

   nfft2=nfft/2

!  Initialize some variables

   df = 0.5d0/dble(ne - 1)	! assume unit sampling
   do k = 1,kspec
      sk(:,k) = (abs(yk(1:ne,k)))**2
   enddo

   varsk = (sk(1,:) + sk(ne,:) + 2.d0*sum(sk(2:ne-1,:),1) ) * df
   dvar = sum(varsk)/dble(kspec)

   bk	 = dvar  * evalu1
   evalu = 1.d0 - evalu1
   sqev  = sqrt(evalu)

   rerr = 9.5d-7	! Previously used r1mach function

!  Do simple jackknife

   do i = 1,kspec

      ks = 0
      do k = 1,kspec

	  if (k == i) then
	     cycle
	  endif
	  ks = ks + 1

         wjk(:,ks) = wt(:,k)
         sj(:,ks) = wjk(:,ks)**2 * sk(:,k)   
      enddo
      
      sjk(:,i) = sum(sj,dim=2)/ sum(wjk**2, dim=2) 

   enddo 

!  Jackknife mean (Log S)

   lspec = log(spec)
   lsjk  = log(sjk)

   lsjk_mean = sum(lsjk, dim=2)/dble(kspec)

!  Jackknife Bias estimate (Log S)

   bjk = dble(kspec-1) * (lspec - lsjk_mean)

!  Jackknife Variance Estimate (Log S)

   do i = 1,kspec
      varjk(:,i) = (lsjk(:,i) - lsjk_mean)**2
   enddo
   var = sum(varjk, dim=2) * dble(kspec-1)/dble(kspec)

!   Use the degrees of freedom

   do i = 1,ne
      if (se(i)<1.d0) then
         write(6,'(a,i5,a,f10.5)') 'dof < 1 ', i, 'th frequency',se(i)
	 stop
      endif
      var(i) = exp( dble(qtdis(0.950, real(se(i)) )) *sqrt(var(i)))
   enddo

!  The setting of the confidence interval is done in mtspec.f90.

   err(:,1) = var
   err(:,2) = var
   
   return
      
end subroutine jackspec



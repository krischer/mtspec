subroutine adaptspec(nfft,ne,kspec,evalu1,yk,	&
                    spec,se,wt)

!
!  subroutine to calculate adaptively weighted power spectrum
!  A second subroutine below, allows for a simple non-adaptive
!  spectrum. Subroutine noadaptspec is below.
!
!  inputs:
!
!    nfft  - number of points in fft's
!    ne    - number of elements in fft to process, normally nfft/2+1
!            for real data and nfft for complex data
!    kspec - number of prolate windows used
!    evalu1- double precision vector containing 1 minus eigenvalues
!    yk    - complex array containing kspec ffts of length nfft
!
!  outputs:
!
!    spec - real vector containing adaptively weighted spectrum
!    se   - real vector containing the number of degrees of freedom
!           for the spectral estimate at each frequency.
!    wt   - real array containing the ne weights for kspec 
!           eigenspectra normalized so that if there is no bias, the
!           weights are unity.
!
!
!  Modified 
!
!	German Prieto
!	Aug 2006
!	
!	    Corrected the estimation of the dofs se(1:ne)
!           (set sum(wt**2,dim=2) = 1.), with maximum wt=1.
!
!	*******************************************************************
!
!	German Prieto
!	October 2007
!		
!	   Added the an additional subroutine noadaptspec to 
!	   calculate a simple non-adaptive multitaper spectrum.
!	   This can be used in transfer functions and deconvolution, 
!	   where adaptive methods might not be necesary. 
!	
!	*******************************************************************
!

!
!  calls nothing
!

!**********************************************************************

   implicit none

!  Input Output

   integer, intent(in) :: nfft, ne, kspec
   real(8), intent(in), dimension(kspec) :: evalu1
   complex(8), intent(in), dimension(nfft,kspec) ::  yk

   real(8), intent(out), dimension(ne) :: spec, se 
   real(8), intent(out), dimension(ne,kspec) :: wt

!  Iterations

   real(8), dimension(ne)       :: sbar, slast
   real(8), dimension(ne,kspec) :: sk, skw
   real(8), dimension(ne,kspec) :: wt_dofs

!  Other variables

   integer :: i, j, k, nfft2
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

!
!  step through frequencies and find weighted spectrum iteratively
!  for each
!

   rerr = 9.5d-7	! Previously used r1mach function

!  Do iteration

   sbar = (sk(:,1)+sk(:,2))/2.d0
      
   do j = 1,1000

      slast = sbar

      do k = 1,kspec
         wt(:,k) = sqev(k)*sbar /( evalu(k)*sbar + bk(k))
         wt(:,k) = dmin1(wt(:,k),1.d0)
         skw(:,k) = wt(:,k)**2 * sk(:,k)   
      enddo
      
      sbar = sum(skw,dim=2)/ sum(wt**2, dim=2)
	    
      if (j==1000) then
         spec = sbar
         write(6,'(a,2E16.7)') 'adaptspec did not converge, rerr = ',    &
	      maxval(abs((sbar-slast)/(sbar+slast))),rerr
	 cycle
      endif
      if (maxval(abs((sbar-slast)/(sbar+slast))) .gt. rerr) then
         cycle
      endif
            
      spec = sbar
      exit
	 
   enddo 

   do i = 1,ne
      wt_dofs(i,:) = wt(i,:)/sqrt(sum(wt(i,:)**2)/dble(kspec))
   enddo

   wt_dofs = dmin1(wt_dofs,1.d0)

   se = 2.d0 * sum(wt_dofs**2, dim=2) 
!   se = 2.d0 * sum(wt**2, dim=2) 

   return
      
end subroutine adaptspec

!--------------------------------------------------------------------
! Constant weighting of tapers
!--------------------------------------------------------------------

subroutine noadaptspec(nfft,ne,kspec,yk,	&
                    spec,se,wt)

!
!  Modified
!	German Prieto
!	Aug 2007
!	
!
!  subroutine to calculate constant weighted power spectrum
!
!  inputs:
!
!    nfft  - number of points in fft's
!    ne    - number of elements in fft to process, normally nfft/2+1
!            for real data and nfft for complex data
!    kspec - number of prolate windows used
!    yk    - complex array containing kspec ffts of length nfft
!
!  outputs:
!
!    spec - real vector containing constant weighted spectrum
!    se   - real vector containing the number of degrees of freedom
!           for the spectral estimate at each frequency.
!    wt   - real array containing the ne weights for kspec 
!           eigenspectra normalized so that if there is no bias, the
!           weights are unity.
!

!**********************************************************************

   implicit none

!  Input Output

   integer, intent(in) :: nfft, ne, kspec
   complex(8), intent(in), dimension(nfft,kspec) ::  yk

   real(8), intent(out), dimension(ne) :: spec, se 
   real(8), intent(out), dimension(ne,kspec) :: wt

!  Iterations

   real(8), dimension(ne,kspec) :: sk

!  Other variables

   integer :: k

!**********************************************************************

!  Initialize some variables

   wt = 1.d0
   do k = 1,kspec
      sk(:,k) = (abs(yk(1:ne,k)))**2
   enddo
   spec = sum(sk,dim=2)/ dble(kspec)

   se = 2.d0 * dble(kspec) 

   return
      
end subroutine noadaptspec




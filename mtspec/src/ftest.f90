subroutine ftest(npts,nf,kspec,vn,yk,F)

!
!  Modiefied 
!	German Prieto
!	April 2006
!
!  Compute F-test for single spectral line components
!  at the frequency bins given by the mtspec routines. 
!  
!  INPUT
!  	npts	number of time points
!	nf	positive frequency points
!	kspec	tapers used in the mtspec
!	vn	Slepian sequences real(8)
!		(npts,kspec)
!	yk	kspec fft's of tapered data series 
!		(npts,kspec), complex(8)
!	
!  OUTPUT
!	F(nf)	vector of f-test values, real(8)
!
!  calls nothing
!
!********************************************************************

   implicit none

!  Input

   integer, intent(in)                            :: npts, nf, kspec
   real(8), dimension(npts,kspec), intent(in)     :: vn
   complex(8), dimension(npts,kspec), intent(in)  :: yk

!  Output

   real(8), dimension(nf), intent(out)            :: F

!  Other

   integer :: i

   real(8), dimension(kspec) :: vn0
   complex(8), dimension(nf) :: mu

!********************************************************************

!  The Vk(0), summing the time domain tapers
!  Also normalize by sum(vn0)**2

   vn0 = sum(vn,dim=1)

!  Calculate the mean amplitude of line components at each frequency

   do i = 1,nf

      mu(i) = sum(vn0*yk(i,:)) / sum(vn0**2)

   enddo

!  Calculate F Test
!		Top	(kspec-1) mu**2 sum(vn0**2)  Model variance
!		Bottom	sum(yk - mu*vn0)**2	 Misfit

   do i = 1,nf

      F(i) = (dble(kspec-1) * abs(mu(i))**2 * sum(vn0**2)) 
      F(i) = F(i) / sum( abs(yk(i,:) - mu(i)*vn0(:))**2 )

   enddo

end subroutine ftest

!--------------------------------------------------------------------
! The padding version of ftest
!--------------------------------------------------------------------

subroutine ftest_pad(npts,nfft,nf,kspec,vn,yk,F)

!
!  Modiefied 
!	German Prieto
!	February 2007
!
!  Compute F-test for single spectral line components
!  at the frequency bins given by the mtspec routines. 
!  
!  INPUT
!  	npts	number of time points
!       nfft    number of frequency points in complex spectra
!	nf	positive frequency points
!	kspec	tapers used in the mtspec
!	vn	Slepian sequences real(8)
!		(npts,kspec)
!	yk	kspec fft's of tapered data series 
!		(nfft,kspec), complex(8)
!	
!  OUTPUT
!	F(nf)	vector of f-test values, real(8)
!
!  calls nothing
!
!********************************************************************

   implicit none

!  Input

   integer, intent(in)                      :: npts, nfft, nf, kspec

   real(8), dimension(npts,kspec), intent(in)     :: vn
   complex(8), dimension(nfft,kspec), intent(in)  :: yk

!  Output

   real(8), dimension(nf), intent(out)            :: F

!  Other

   integer :: i

   real(8), dimension(kspec) :: vn0
   complex(8), dimension(nf) :: mu

!********************************************************************

!  The Vk(0), summing the time domain tapers
!  Also normalize by sum(vn0)**2

   vn0 = sum(vn,dim=1)

!  Calculate the mean amplitude of line components at each frequency

   do i = 1,nf

      mu(i) = sum(vn0*yk(i,:)) / sum(vn0**2)

   enddo

!  Calculate F Test
!		Top	(kspec-1) mu**2 sum(vn0**2)  Model variance
!		Bottom	sum(yk - mu*vn0)**2	 Misfit

   do i = 1,nf

      F(i) = (dble(kspec-1) * abs(mu(i))**2 * sum(vn0**2)) 
      F(i) = F(i) / sum( abs(yk(i,:) - mu(i)*vn0(:))**2 )

   enddo

end subroutine ftest_pad






















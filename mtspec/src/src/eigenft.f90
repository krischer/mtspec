subroutine eigenft(npts, x, kspec, vn, lambda, nf, yk, sk, sbar)

!
!  Modified
!	German Prieto
!	November 2004
!
!  Subroutine to get the eigenft's , yk's from
!  Thomson (1982). It also computes the mean 
!  spectra Sbar, by averaging the different eigenspectra, 
!  with lambda as weights.
!
!  computes eigen-ft's by windowing real data with dpss and taking ffts
!  note that fft is unnormalized and window is such that
!  its sum of squares is one, so that psd=yk**2
!  the fft's are computed in pairs by placing adjacent dpss*data
!  in the real part of a complex vector, taking the fft and retrieving
!  the yk's. The yk's are saved to get phase information.
!
! 
!  Input:
!
!	npts		number of data points in time series
!	x(npts)		real vector with the time series
!	kspec		number of tapers to use
!	vn(npts,kspec)	the different tapers computed in dpss
!	lambda(kspec)	the eigenvalues of the tapers vn
!	nf		number of frequency points (usually npts/2 + 1
!
!  Output:
!	
!	yk(kspec,npts)	complex array with kspec fft's of tapered 
!			data. If data is real, only nf points are 
!			needed, but I save all just to be sure. 
!	sk(kspec,nf)	real array with kspec eigenspectra of length
!			nf.
!	sbar(nf)	real array with the mean spectral estimate.
!			weigthed with 1/lambda. 
!
!  calls fft
!

!**********************************************************************

   implicit none

!  Time series

   integer, intent(in) :: npts

   real(8), intent(in), dimension(npts) :: x
  
!  Frequency variables

   integer, intent(in) :: nf

!  Eigenspectra

   integer, intent(in) :: kspec
   
   real(8), intent(out), dimension(nf,kspec) :: sk
   
   complex(8), intent(out), dimension(npts,kspec) :: yk 

!  Simple mean spectra

   real(8), intent(out), dimension(nf) :: sbar

!  Dpss 

   real(8), intent(in), dimension(kspec) :: lambda 
   
   real(8), intent(in), dimension(npts,kspec) :: vn

!  Extras

   real(8), dimension(2,npts) :: a
   
   integer :: i

   complex(8), dimension(npts) :: dummy_yk


!********************************************************************

!
!  Multiply time series x with the appropiate taper vn
!  Take the FFT, and save the  	eigencoefficients yk, 
!				eigenspectra      sk,
!				mean spectra 	  sbar
!

   do i = 1,kspec
   
      a(1,:) = vn(:,i)*x(:)
      a(2,:) = 0.d0

      dummy_yk = cmplx(a(1,:),a(2,:),kind=8)
      
      call fft(dummy_yk,npts)
      
      yk(:,i) = dummy_yk(:)

      sk(1:nf,i) = (abs(yk(1:nf,i)))**2

   enddo

   sbar = 0.d0

   do i = 1, kspec
      sbar(:) = sbar(:) + ((1.d0/lambda(i)) * sk(:,i))
   enddo

!  Two power for one sided spectra

   sbar = (2.d0/kspec)*sbar

   
   return
   
end subroutine eigenft

!--------------------------------------------------------------------
! The padding version
!--------------------------------------------------------------------

subroutine eigenft_pad(npts, nfft, x, kspec, vn, lambda, nf, yk, sk, sbar)

!
!  Modified
!	German Prieto
!	February 2007
!
!  Subroutine to get the eigenft's , yk's from
!  Thomson (1982). It also computes the mean 
!  spectra Sbar, by averaging the different eigenspectra, 
!  with lambda as weights.
!  This subroutines add zero padding to the time series
!  after tapering, so as to increase the number of points
!  in the frequency spectrum to nfft. 
!
!  computes eigen-ft's by windowing real data with dpss, 
!  PADDING with NFFT-NPTS zeros and taking ffts
!  note that fft is unnormalized and window is such that
!  its sum of squares is one, so that psd=yk**2
!  the fft's are computed in pairs by placing adjacent dpss*data
!  in the real part of a complex vector, taking the fft and retrieving
!  the yk's. The yk's are saved to get phase information.
!
! 
!  Input:
!
!	npts		number of data points in time series
!	x(npts)		real vector with the time series
!	kspec		number of tapers to use
!	vn(npts,kspec)	the different tapers computed in dpss
!	lambda(kspec)	the eigenvalues of the tapers vn
!	nf		number of frequency points (usually npts/2 + 1
!
!  Output:
!	
!	yk(kspec,nfft)	complex array with kspec fft's of tapered 
!			data. If data is real, only nf points are 
!			needed, but I save all just to be sure. 
!	sk(kspec,nf)	real array with kspec eigenspectra of length
!			nf.
!	sbar(nf)	real array with the mean spectral estimate.
!			weigthed with 1/lambda. 
!
!  calls fft
!

!**********************************************************************

   implicit none

!  Time series

   integer, intent(in) :: npts, nfft

   real(8), intent(in), dimension(npts) :: x
  
!  Frequency variables

   integer, intent(in) :: nf

!  Eigenspectra

   integer, intent(in) :: kspec
   
   real(8), intent(out), dimension(nf,kspec) :: sk
   
   complex(8), intent(out), dimension(nfft,kspec) :: yk 

!  Simple mean spectra

   real(8), intent(out), dimension(nf) :: sbar

!  Dpss 

   real(8), intent(in), dimension(kspec) :: lambda 
   
   real(8), intent(in), dimension(npts,kspec) :: vn

!  Extras

   real(8), dimension(2,nfft) :: a
   
   integer :: i

   complex(8), dimension(nfft) :: dummy_yk


!********************************************************************

!
!  Multiply time series x with the appropiate taper vn
!  Take the FFT, and save the  	eigencoefficients yk, 
!				eigenspectra      sk,
!				mean spectra 	  sbar
!

   do i = 1,kspec

      a = 0.d0 
      a(1,1:npts) = vn(:,i)*x(:)
      a(2,:) = 0.d0

      dummy_yk = cmplx(a(1,:),a(2,:),kind=8)
      
      call fft(dummy_yk,nfft)
      
      yk(:,i) = dummy_yk(:)

      sk(1:nf,i) = (abs(yk(1:nf,i)))**2

   enddo

   sbar = 0.d0

   do i = 1, kspec
      sbar(:) = sbar(:) + ((1.d0/lambda(i)) * sk(:,i))
   enddo

!  Two power for one sided spectra

   sbar = (2.d0/kspec)*sbar

   return
   
end subroutine eigenft_pad

!--------------------------------------------------------------------
! The complex version
!--------------------------------------------------------------------

subroutine eigenft_c(npts, x, kspec, vn, lambda, nf, yk, sk, sbar)

!**********************************************************************

   implicit none

!  Time series

   integer, intent(in) :: npts

   complex(8), intent(in), dimension(npts) :: x
  
!  Frequency variables

   integer, intent(in) :: nf

!  Eigenspectra

   integer, intent(in) :: kspec
   
   real(8), intent(out), dimension(nf,kspec) :: sk
   
   complex(8), intent(out), dimension(npts,kspec) :: yk 

!  Simple mean spectra

   real(8), intent(out), dimension(nf) :: sbar

!  Dpss 

   real(8), intent(in), dimension(kspec) :: lambda 
   
   real(8), intent(in), dimension(npts,kspec) :: vn

!  Extras

   complex(8), dimension(npts) :: a
   
   integer :: i

   complex(8), dimension(npts) :: dummy_yk


!********************************************************************

!
!  Multiply time series x with the appropiate taper vn
!  Take the FFT, and save the  	eigencoefficients yk, 
!				eigenspectra      sk,
!				mean spectra 	  sbar
!

   if (nf .ne. npts) then
      write(6,*) 'Error in eigenft_c, nf not equal npts'
      stop
   endif

   do i = 1,kspec
   
      a = vn(:,i)*x(:)

      dummy_yk = cmplx(a,kind=8)
      
      call fft(dummy_yk,npts)
      
      yk(:,i) = dummy_yk(:)

      sk(1:nf,i) = (abs(yk(1:nf,i)))**2

   enddo

   sbar = 0.d0

   do i = 1, kspec
      sbar(:) = sbar(:) + ((1.d0/lambda(i)) * sk(:,i))
   enddo

!  Two power for one sided spectra

   sbar = (2.d0/kspec)*sbar
   
   return
   
end subroutine eigenft_c










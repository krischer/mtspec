subroutine sine_psd (npts,dt,x,ntap,ntimes,fact,nf,freq,spec,kopt,err)

!
!  Performs the PSD estimation by the sie multitaper method of
!  Riedel and Sidorenko, IEEE Tr. Sig. Pr, 43, 188, 1995
!
!  Based on Bob Parker psd.f codes. Most of the comments come 
!  his documentation as well.
!
!  Last Modified:
!	German Prieto
!	September 22 2005
!	
!  The subroutine is in charge of estimating the adaptive sine 
!  multitaper as in Riedel and Sidorenko (1995). 
!  This is done by performing a MSE adaptive estimation. First
!  a pilot spectral estimate is used, and S" is estimated, in 
!  order to get te number of tapers to use, using (13) of 
!  R & S for a min suqare error spectrum. 
!  Unlike the prolate spheroidal multitapers, the sine multitaper 
!  adaptive process introduces a variable resolution and error in 
!  the frequency domain. Complete error information is contained 
!  in the output variables file as the corridor of 1-standard-deviation 
!  errors, and in K, the number of tapers used at each frequency.
!  The errors are estimated in the simplest way, from the number of 
!  degrees of freedom (two per taper), not by jack-knifing. The 
!  frequency resolution is found from K*fN/Nf where fN is the Nyquist 
!  frequency and Nf is the number of frequencies estimated.
!  The adaptive process used is as follows. A quadratic fit to the
!  log PSD within an adaptively determined frequency band is used 
!  to find an estimate of the local second derivative of the 
!  spectrum. This is used in an equation like R & S (13) for the 
!  MSE taper number, with the difference that a parabolic weighting 
!  is applied with increasing taper order. Because the FFTs of the 
!  tapered series can be found by resampling the FFT of the original 
!  time series (doubled in length and padded with zeros) only one FFT 
!  is required per series, no matter how many tapers are used. This 
!  makes the program fast. Compared with the Thomson multitaper 
!  programs, this code is not only fast but simple and short. The 
!  spectra associated with the sine tapers are weighted before 
!  averaging with a parabolically varying weight. The expression 
!  for the optimal number of tapers given by R & S must be modified
!  since it gives an unbounded result near points where S" vanishes,
!  which happens at many points in most spectra. This program 
!  restricts the rate of growth of the number of tapers so that a 
!  neighboring covering interval estimate is never completely 
!  contained in the next such interval.
!  This method SHOULD not be used for sharp cutoffs or deep 
!  valleys, or small sample sizes. Instead use Thomson multitaper
!  in mtspec in this same library. 
!
!  INPUT
!
!	npts		integer number of points in time series
! 	dt		real, sampling spacing of time series
!	x(npts)		real, data vector
!	ntap		integer, constant number of tapers (def = 0)
!	ntimes		integer, number of iterations to perform
!	fact		real, degree of smoothing (def = 1.)
!	nf		integer, number of freq points in spectrum
!
!  OUTPUT
!
!	freq(nf)	real vector with frequency bins
!	spec(nf)	real vector with the adaptive estimated spectrum
!
!  OPTIONAL OUTPUT
!
!	kopt(nf)	integer, number of taper per freq point
!	err(nf,2)	1-std errors (simple dof estimate)
!	
!  calls quick, adapt
!

!********************************************************************

   implicit none

!  Input 

   integer, intent(in) :: npts, nf 
   integer, intent (in out) :: ntap, ntimes

   real(4), intent(in) :: dt
   real(4), intent(in out) ::  fact

   real(4), intent(in), dimension(npts) :: x

!  Output 

   real(4), intent(out), dimension(nf)    :: spec, freq

!  Optional

   integer, dimension(nf),       optional :: kopt
   integer, dimension(nf)                 :: kopt_o

   real(4), dimension(nf,2),     optional :: err
   
!  Freq variables

   integer :: nptwo
   real(4) :: df, fnyq
   complex(4), dimension(:), allocatable :: fx
   real(4), dimension(npts)              :: x2
   real(4), dimension(:), allocatable    :: dfx

   real(4), dimension(20)                :: ar

!  Adapt variables

   integer :: initap

!  Other

   integer :: i
   real(4) :: const
   real(4), dimension(nf) :: v, std
   real(4) :: xmean, xvar
   
!********************************************************************

   write(6,'(a)') 'Calling Sine multitaper'

!  Set defaults

   if (ntap<2) then
      ntap = 0
   endif
   if (ntimes<=0) then
      ntimes = 2 
   endif
   if (fact<=0.) then
      fact = 1.
   endif

!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean = sum(x)/real(npts)
   xvar  = (sum((x - xmean)**2))/real(npts-1)
   x2    = x - xmean

!
! Frequency variables
!

   fnyq = 0.5/dt
   df   = fnyq/real(nf - 1)

!
!  Define frequency bins
!

   do i = 1,nf
      freq(i) = real(i-1)*df
   enddo

!
!  Get the FFT once
!

   nptwo = 2*npts
   allocate(fx(nptwo))
   allocate(dfx(nptwo))


   dfx(1:npts) = x2(1:npts)
   dfx(npts+1:nptwo) = 0.0

   call fft_r(dfx,nptwo,fx)

!  Pre-Whitening
!
!   call quick(nptwo,fx,nf,15,spec,kopt_o)
!   call yule(1, 3,npts,x2,nf,spec,ar)
!
!   fx(1:npts) = cmplx(x2(1:npts))
!   fx(npts+1:nptwo) = 0.
!
!   call fft4(fx,nptwo)
   ar = 0.

!
!  Check if constant tapers or adaptive method
!

   if (ntap>0) then

   !  Estimate uniform taper PSD

      write(6,'(a,i6,a)') 'Uniform S(f) with ',ntap, ' tapers'
      write(6,'(a,f6.1)') 'Time-bandwidth product ',0.5*ntap

      call quick(nptwo,fx,nf,ntap,spec,kopt_o)

   else

      initap = 3.0 + sqrt(fact*real(npts))/5.0

      write(6,'(a,i6,a)') 'Adaptive S(f) with ',ntimes, ' iterations'
      write(6,'(a,i6)') 'Initial number of tapers ', initap

      call adapt(nptwo,fx,nf,df,initap,ntimes,fact,spec,kopt_o)

   endif 

!  Undo the prewhitening
!
!   call yule(2, 3,npts,x2,nf,spec,ar)

!  Normalize the spectrum

   const = xvar/(sum(spec)*df)
      
   spec = const*spec

!
!  Error estimate
!  The 5 - 95% confidence limits are estimated using the 
!  approximation of Chambers et al, 1983 Graphical Methods
!  for data Analysis. See also Percival and Walden p. 256, 1993
!  The 1.2 factor comes from the parabolic weighting.
!


   if (present(err)) then
      std =  spec / sqrt(real(kopt_o)/1.2)    ! The 1 standard deviation
      v   =  2.*real(kopt_o)/1.2              ! Degrees of freedom

      err(:,1) = spec / (1-2./real(9*v)-1.96*sqrt(2./real(9*v)))**3;
      err(:,2) = spec / (1-2./real(9*v)+1.96*sqrt(2./real(9*v)))**3;
   endif

   if (present(kopt)) then
      kopt = kopt_o
   endif

   deallocate(fx) 

end subroutine sine_psd

!--------------------------------------------------------------------
! A short version of outputs for sine_psd
!--------------------------------------------------------------------

subroutine sine_psd_short (npts,dt,x,ntap,ntimes,fact,nf,spec)

!
!  Performs the PSD estimation by the sine multitaper method of
!  Riedel and Sidorenko, IEEE Tr. Sig. Pr, 43, 188, 1995
!
!  Based on Bob Parker psd.f codes. Most of the comments come 
!  his documentation as well.
!
!  Last Modified:
!	German Prieto
!	September 22 2005
!
!	German A. Prieto
!	July 24 2007.
!		Changed 2 of the output variables, to run 
!		faster and no print statements allowed.
!	
!  INPUT
!
!	npts		integer number of points in time series
! 	dt		real, sampling spacing of time series
!	x(npts)		real, data vector
!	ntap		integer, constant number of tapers (def = 0)
!	ntimes		integer, number of iterations to perform
!	fact		real, degree of smoothing (def = 1.)
!	nf		integer, number of freq points in spectrum
!
!  OUTPUT
!
!	spec(nf)	real vector with the adaptive estimated spectrum
!	
!  calls quick, adapt
!

!********************************************************************

   implicit none

!  Input 

   integer, intent(in)      :: npts, nf 
   integer, intent (in out) :: ntap, ntimes

   real(4), intent(in)      :: dt
   real(4), intent(in out)  :: fact

   real(4), intent(in), dimension(npts) :: x

!  Output 

   real(4), intent(out), dimension(nf)   :: spec
   
!  Freq variables

   integer :: nptwo
   real(4) :: df, fnyq
   integer,    dimension(:), allocatable      :: kopt
   complex(4), dimension(:), allocatable      :: fx
   real(4),    dimension(:), allocatable      :: x2
   real(4),    dimension(:), allocatable      :: dfx

!  Adapt variables

   integer :: initap

!  Other

   real(4) :: const
   real(4) :: xmean, xvar
   
!********************************************************************

   allocate(x2(npts))
   allocate(fx(2*npts))
   allocate(dfx(2*npts))
   allocate(kopt(nf))

!  Set defaults

   if (ntap < 2) then
      ntap = 0
   endif
   if (ntimes <= 0) then
      ntimes = 2 
   endif
   if (fact <= 0.) then
      fact = 1.
   endif

!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean = sum(x)/real(npts)
   xvar  = (sum((x - xmean)**2))/real(npts-1)
   x2    = x - xmean

!
! Frequency variables
!

   fnyq = 0.5/dt
   df   = fnyq/real(nf - 1)

!
!  Get the FFT once
!

   nptwo = 2*npts

   dfx(1:npts) = x2(1:npts)
   dfx(npts+1:nptwo) = 0.0

   call fft_r(dfx,nptwo,fx)

!
!  Check if constant tapers or adaptive method
!

   if (ntap>0) then

   !  Estimate uniform taper PSD

      call quick(nptwo,fx,nf,ntap,spec,kopt)

   else

      initap = 3.0 + sqrt(fact*real(npts))/5.0

      call adapt(nptwo,fx,nf,df,initap,ntimes,fact,spec,kopt)

   endif 

!  Normalize the spectrum

   const = xvar/(sum(spec)*df)
      
   spec = const*spec

   deallocate(x2, fx, kopt)

   return

end subroutine sine_psd_short

!--------------------------------------------------------------------
!  The adaptive subroutines
!--------------------------------------------------------------------

subroutine quick(nptwo,fx,nf,ktop,spec,kopt)

!
!  Sine multitaper routine. With a double length FFT constructs
!  FT[sin(q*n)*x(n)] from F[x(n)], that is constructs the 
!  FFT of the sine tapered signal. 
!  The FFT should be performed previous to the call. 
!  
!  INPUT
!	nptwo		The twice signal length (2*npts)
!	fx		The FFT of the signal (twice length)
!	nf		Number of frequency points for spec
!	ktop		if > 0  Constant value to be used
!			if <= 0 Use the kopt array instead
! 
!  OUTPUT
!	spec(nf)	the spectral estimate
!
!  INPUT/OUTPUT
!	kopt		number of tapers per frequency. 
!			if ktop>0 uses a constant value.
!
!  Based on the sine multitaper code of R. L. Parker.
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in)    :: nptwo, nf, ktop
   complex(4), intent(in), dimension(nptwo) :: fx

!  In out

   integer, intent(in out), dimension(nf) :: kopt

!  Out

   real(4), intent(out), dimension(nf) :: spec

!  Others

   integer :: m, m2, klim, k, j1, j2
   real(4) :: ck, wk

   complex(4) :: zz

!********************************************************************

   if (ktop>0) then
      kopt = ktop
   endif

!  Loop over frequency

   do m = 1,nf
      
      m2 = 2* (m-1)

      spec(m) = 0.
      klim = kopt(m)
      ck = 1./real(klim)**2

!  Average over tapers, parabolic weighting wk

      do k = 1,klim
      
         j1 = mod(m2+nptwo-k,nptwo)
         j2 = mod(m2+k,nptwo)

         zz = fx(j1+1) - fx(j2+1)
         wk = 1. - ck*real(k-1)**2

         spec(m) = spec(m) + (real(zz)**2 + aimag(zz)**2) * wk

      enddo

!  Exact normalization for parabolic factor

      spec(m) = spec(m) * (6.0*real(klim))/real(4*klim**2+3*klim-1)

   enddo

   return

end subroutine quick

!--------------------------------------------------------------------

subroutine adapt(nptwo,fx,nf,df,initap,ntimes,fact,spec,kopt)

!
!  Performs the adaptive spectral estimation
!  From a basic pilot estimate, computes S" to be used
!  in (13) of Riedel and Sidorenko (1995) for the 
!  MSE spectrum.
!
!  INPUT
!	nptwo		The twice signal length (2*npts)
!	fx		The FFT of the signal (twice length)
!	nf		Number of frequency points for spec
!       df		Freq sampling
!	initap		Number of tapers to use for pilot estimate
!			Later we can add the mtspec result as test
!	ntimes		number of iterations for estimate
!       fact		degree of smoothing (def = 1.0)
! 
!  OUTPUT
!	spec(nf)	the spectral estimate
!	kopt(nf)	number of tapers per frequency. 
!
!  Based on the sine multitaper code of R. L. Parker.
!	
!  calls quick, north, curb

!********************************************************************
 
   implicit none

!  Input

   integer, intent(in)    :: nptwo, nf, initap, ntimes
   complex(4), intent(in), dimension(nptwo) :: fx
   real(4), intent(in) :: fact, df

!  Out

   integer, intent(out), dimension(nf) :: kopt
   real(4), intent(out), dimension(nf) :: spec

!  Others

   integer :: iter, j, ispan
   real(4) :: c1, c2, d1, d2, R, ak, phi, sigR
   real(4), dimension(nf) :: y, opt


!********************************************************************

!  c1, c2=(20*sqrt(1.2))**0.4 are constants for parabolic weighting 
!  in subroutine quick; for uniform weighting c1=1, c2=12.0**0.4=2.702
   
   c1=1.2000 
   c2=3.437

!  Get pilot estimate
   
   call quick(nptwo,fx,nf,initap,spec,kopt)

!
!  Do the adaptive estimate. Find MSE iteratively.
!  Estimate number of tapers at each freq for MSE spectrum
!  Estimate 2nd derivative of S from ln S:
!  To find |S"/S| use |theta"| + (theta')**2, theta=ln S
!

   do iter = 1,ntimes

      do j = 1,nf
         y(j) = log(spec(j))
      enddo

!
!  Estimate K, number of tapers at each freq for MSE spectrum
!  R = S"/S -- use R = Y" + (Y')**2 , Y=ln S.
!  Note  c2=3.437
!

      do j = 1,nf
      
         ispan = kopt(j)*1.4

         call north(nf,j-ispan, j+ispan, y, d1, d2)

         R = (d2  + d1**2)/df**2
         ak=real(kopt(j))/real(2*ispan)
         phi=720.0*ak**5*(1.0 - 1.286*ak + 0.476*ak**3 - 0.0909*ak**5)
         sigR= sqrt(phi/real(kopt(j))**5) / df**2

         opt(j)=c2/(df**4 *( R**2 + 1.4*sigR**2) /fact**2)** 0.2

      enddo

!  Curb runaway growth of Kopt near zeros of R

      call curb(nf,opt)
     
      do j = 1,nf
         kopt(j) = max(3.0,opt(j))
      enddo

!  Recompute spectrum with optimal variable taper numbers

      call quick(nptwo,fx,nf,0,spec,kopt)

   enddo

   return

end subroutine adapt

!_______________________________________________________________________

subroutine north(n, i1, i2, s, ds, dds)

!
!  Performs LS fit to s by
!  a degree-two polynomial in an orthogonal basis.
!  Returns ds = estimate of 1st derivative  ds/dn  at center of record
!  Returns dds = estimate of 2nd derivative
!

!********************************************************************

   implicit none

   integer :: n, i1, i2
   
   real(4), dimension(*) :: s

   real(4) :: ds, dds

   integer :: L, kk, i

   real(4) :: el, gamma, u0sq, u1sq, u2sq, amid, dot0, dot1, dot2

!********************************************************************

   L = i2 - i1 + 1
   el= real(L)
   gamma = (el**2 - 1.0)/12.0
   
   u0sq = el
   u1sq = el*(el**2 - 1.0)/12.0
   u2sq = (el*(el**2 - 1.0)*(el**2- 4.0))/180.0
   amid= 0.5*(el + 1.0)
   dot0=0.0
   dot1=0.0
   dot2=0.0
   do kk=1, L
      i=kk + i1 - 1

!  Negative or excessive index uses even function assumption

      if (i.le. 0) i=2 - i
      if (i.gt. n) i=2*n - i
      dot0 = dot0 + s(i)
      dot1 = dot1 + (kk - amid) * s(i)
      dot2 = dot2 + ((kk - amid)**2 - gamma)*s(i)
   enddo
      
   ds = dot1/u1sq
   dds = 2.0*dot2/u2sq

   return
      
end subroutine north

!--------------------------------------------------------------------

subroutine curb(n, v)

!
!  Rewrites the input n-vector v() so that all points lie below
!  the piece-wise linear function v(k) + abs(j-k), where v(k)
!  is a local minimum in the original v.
!  Effectively clips strong peaks and keeps slopes under 1 in
!  magnitude.
!

!********************************************************************

   implicit none

   integer, intent(in) :: n
   real(4), dimension(n), intent(in out) :: v

!  Others

   integer :: j, k
   real(4) :: vloc

!********************************************************************


   do j=2, n-1

!     Scan series for local minimum
      if (v(j).lt.v(j+1) .and. v(j).lt.v(j-1)) then

         vloc=v(j)

!        Revise series accordingly
         do k=1, n
            v(k)=min(v(k), vloc+real(abs(j-k)))
        enddo

     endif

   enddo

   return

end subroutine curb

!--------------------------------------------------------------------








subroutine sine_cohe (npts,dt,x1,x2,ntap,ntimes,fact,nf,p,     &
                     freq,cohe,phase,speci,specj,kopt,conf,cohe_ci,phase_ci)

!
!  Performs the coherence estimation by the sine multitaper method of
!  Riedel and Sidorenko, IEEE Tr. Sig. Pr, 43, 188, 1995
!
!  Based on Bob Parker psd.f and cross.f codes. Most of the comments 
!  come from his documentation as well.
!
!  Last Modified:
!	German Prieto
!	September 29 2005
!
!	German A. Prieto
!	September 31, 2007
!
!	Added optional arguments and made part of the spectra.mod
!	module. 
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
! 	dt		real, sampling rate of time series
!	x1(npts)	real, data for first series
!       x2(npts) 	real, data for second series
!	ntap		integer, constant number of tapers (def = 0)
!	ntimes		integer, number of iterations to perform
!	fact		real, degree of smoothing (def = 1.)
!	nf		integer, number of freq points in spectrum
!	p		confidence for null hypothesis test
!
!  OPTIONAL OUTPUT
!
!	freq(nf)	real vector with frequency bins
!       cohe(nf)	real, coherence of the two series (0 - 1)
!       phase(nf)	the phase at each frequency
!	speci(nf)	real vector with spectrum of first series
!	specj(nf)	real vector with spectrum of second
!       kopt(nf)	integer, number of taper per freq point
!	conf(nf)	95% confidence value for each freq.
!	cohe_ci(nf,2)	95% bounds on coherence (not larger than 1)
!       phase_ci(nf,2)  95% bounds on phase estimate
!
!	If confidence intervals are requested, then both phase and
!	cohe variables need to be requested as well. 
!	
!  calls quick2, adapt2, atanh2
!

!********************************************************************

   use spectra

   implicit none

!  Input 

   integer, intent(in) :: npts, nf 
   integer, intent (in out) :: ntap, ntimes

   real(4), intent(in) :: dt 
   real(4), intent(in out) ::  fact, p

   real(4), intent(in), dimension(npts) :: x1, x2

!  Output  Optional

   integer, dimension(nf),   optional   :: kopt
   real(4), dimension(nf),   optional   :: speci, specj, freq
   real(4), dimension(nf),   optional   :: cohe, phase, conf
  
   real(4), dimension(nf,2), optional   :: cohe_ci, phase_ci

!  Freq variables

   real(4), dimension(nf)   :: f
   integer, dimension(nf)   :: ktap

   integer :: nptwo
   real(4) :: df, fnyq
   complex(4), dimension(:,:), allocatable :: fx

   real(4), dimension(nf,4) :: sxy
   real(4), dimension(nf)   :: v

!  Adapt variables

   integer :: initap

!  Other

   integer :: i, j, k
   real(4) :: const, power
   real(4), dimension(2) :: xmean, xvar

   real(4) :: atanh2, qtop, qbot
   real(4) :: Q 

   
!********************************************************************

   write(6,'(a)') 'Calling Sine multitaper coherence'

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
   if (p>=1. .or. p<=0.) then
      p = 0.95
   endif

   write(6,'(a,f10.7)') 'Confidence for null hypothesis testing ', p

!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean(1) = sum(x1)/real(npts)
   xvar(1) = (sum((x1 - xmean(1))**2))/real(npts-1)

   xmean(2) = sum(x2)/real(npts)
   xvar(2) = (sum((x2 - xmean(2))**2))/real(npts-1)

!
! Frequency variables
!

   fnyq	= 0.5/dt
   df 	= fnyq/real(nf - 1)

!
!  Define frequency bins
!

   do i = 1,nf
      f(i) = real(i-1)*df
   enddo

!
!  Get the FFT once
!

   nptwo = 2*npts
   allocate(fx(nptwo,2))

   fx(1:npts,1) = cmplx(x1(1:npts))
   fx(1:npts,2) = cmplx(x2(1:npts))
   fx(npts+1:nptwo,1) = 0.
   fx(npts+1:nptwo,2) = 0.

   call fft4(fx(:,1),nptwo)
   call fft4(fx(:,2),nptwo)

!
!  Check if constant tapers or adaptive method
!

   if (ntap>0) then

   !  Estimate uniform taper PSD

      write(6,'(a,i6,a)') 'Uniform cohe(f) with ',ntap, ' tapers'
      write(6,'(a,f6.1)') 'Time-bandwidth product ',0.5*ntap

      call quick2(nptwo,fx,nf,ntap,sxy,ktap)

   else

      initap = 3.0 + sqrt(fact*real(npts))/5.0

      write(6,'(a,i6,a)') 'Adaptive S(f) with ',ntimes, ' iterations'
      write(6,'(a,i6)') 'Initial number of tapers ', initap

      call adapt2(nptwo,fx,nf,df,initap,ntimes,fact,sxy,ktap)

   endif 

!  Normalize by variance in x1 = area under psd

   power=0.5*(sxy(1,1) + sxy(nf,1))
   do k=2, nf-1
      power=power + sxy(k,1)
   enddo
   const = xvar(1)/(power*df)

   do k=1, nf
      do i=1, 4
         sxy(k,i)=sxy(k,i)*const
      enddo
   enddo

!  Create outputs

   v   =  2.*real(ktap)/1.2     ! degrees of freedom

   do j = 1, nf

      if (present(cohe)) then
         cohe(j)  = (sxy(j,3)**2 + sxy(j,4)**2)/(sxy(j,1)*sxy(j,2))
      endif
      !gain(j)=sqrt(cohe(j)*sxy(j,2)/sxy(j,1))
      if (present(phase)) then
         phase(j) = atan2( sxy(j,4),  sxy(j,3)) 
      endif
      if (present(conf)) then
         conf(j)  = 1. - ( (1.0-p)**(1./(v(j)/2. -1)) )
      endif

   enddo 

   if (present(freq)) then
      freq = f
   endif
   if (present(speci)) then
      speci = sxy(:,1)
   endif
   if (present(specj)) then
      specj = sxy(:,2)
   endif
   if (present(kopt)) then
      kopt = ktap
   endif


   !  Confidence intervals (atanh transformation)
   !  Eq 2.58 Thomson and Chave, 1991 (Jackknife error ...)
   !  Note that the bounds get smaller if coherence is 
   !  close to 1. It seems right, since high coherence means
   !  signals are alike, and there would be less randomness
   !  and simply compare two identical FFTs. 
   !  Note that v ~ 2*m in Eq 2.58

   if (.not. present(cohe_ci) .or. .not. present(phase_ci)) then
      if (present(phase)) then
         phase = phase * (180.0/3.1415926535)
      endif
      deallocate(fx)
      return
   endif

   if (.not. present(cohe) .or. .not. present(phase)) then
      cohe_ci  = 0.
      phase_ci = 0.
      if (present(phase)) then
         phase = phase * (180.0/3.1415926535)
      endif
      deallocate(fx)
      return
   endif

   do j = 1,nf
      Q = atanh2(sqrt(cohe(j))) * sqrt(v(j)-2)  
      qtop = Q+1.96
      qbot = Q-1.96
      if (qbot < 0.) then
         qbot = 0.
      endif
      cohe_ci(j,1) = tanh( qbot/sqrt(v(j)-2) )**2
      cohe_ci(j,2) = tanh( qtop/sqrt(v(j)-2) )**2

      phase_ci(j,1) = phase(j) - 2.*sqrt( (2./v(j))*(1./cohe(j) - 1.) )
      phase_ci(j,2) = phase(j) + 2.*sqrt( (2./v(j))*(1./cohe(j) - 1.) )

   enddo

   phase = phase * (180.0/3.1415926535)
   phase_ci = phase_ci * (180.0/3.1415926535)

   deallocate(fx)

   return

end subroutine sine_cohe

!--------------------------------------------------------------------
!  The adaptive subroutines
!--------------------------------------------------------------------

subroutine quick2(nptwo,fx,nf,ktop,sxy,kopt)

!
!  Sine multitaper routine. With a double length FFT constructs
!  FT[sin(q*n)*x(n)] from F[x(n)], that is constructs the 
!  FFT of the sine tapered signal. 
!  The FFT should be performed previous to the call. 
!  
!  INPUT
!	nptwo		The twice signal length (2*npts)
!	fx(nptwo,2)	The FFT of the two signals (twice length)
!	nf		Number of frequency points for spec
!	ktop		if > 0  Constant value to be used
!			if <= 0 Use the kopt array instead
! 
!  OUTPUT
!	sxy(nf,4)	the spectral estimate
!
!  INPUT/OUTPUT
!	kopt		number of tapers per frequency. 
!			if ktop>0 uses a constant value.
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in)    :: nptwo, nf, ktop
   complex(4), intent(in), dimension(nptwo,2) :: fx

!  In out

   integer, intent(in out), dimension(nf) :: kopt

!  Out

   real(4), intent(out), dimension(nf,4) :: sxy

!  Others

   integer :: m, m2, klim, k, j1, j2
   real(4) :: ck, wk

   complex(4) :: z1, z2

!********************************************************************

   if (ktop>0) then
      kopt = ktop
   endif

!  Loop over frequency

   do m = 1,nf
      
      m2 = 2* (m-1)

      sxy(m,:) = 0.

      klim = kopt(m)
      ck = 1./real(klim)**2

!  Average over tapers, parabolic weighting wk

      do k = 1,klim
      
         j1 = mod(m2+nptwo-k,nptwo)
         j2 = mod(m2+k,nptwo)

         z1 = fx(j1+1,1) - fx(j2+1,1)
         z2 = fx(j1+1,2) - fx(j2+1,2)

         wk = 1. - ck*real(k-1)**2

         sxy(m,1) = sxy(m,1) + (real(z1)**2 + aimag(z1)**2) * wk
         sxy(m,2) = sxy(m,2) + (real(z2)**2 + aimag(z2)**2) * wk
         sxy(m,3) = sxy(m,3) +    &
                    (real(z1)*real(z2) + aimag(z1)*aimag(z2)) * wk
         sxy(m,4) = sxy(m,4) +    & 
                    (real(z2)*aimag(z1) - real(z1)*aimag(z2)) * wk
         
      enddo

!  Exact normalization for parabolic factor

      sxy(m,:) = sxy(m,:) * (6.0*real(klim))/real(4*klim**2+3*klim-1)

   enddo

   return

end subroutine quick2

!--------------------------------------------------------------------

subroutine adapt2(nptwo,fx,nf,df,initap,ntimes,fact,sxy,kopt)

!
!  Performs the adaptive spectral estimation
!  From a basic pilot estimate, computes S" to be used
!  in (13) of Riedel and Sidorenko (1995) for the 
!  MSE spectrum.
!
!  INPUT
!	nptwo		The twice signal length (2*npts)
!	fx(nptwo,2)	The FFT of the signal (twice length)
!	nf		Number of frequency points for spec
!	df		Freq sampling
!	initap		Number of tapers to use for pilot estimate
!			Later we can add the mtspec result as test
!	ntimes		number of iterations for estimate
!       fact		degree of smoothing (def = 1.0)
! 
!  OUTPUT
!	sxy(nf,4)	the spectral estimates and coherence, phase
!	kopt(nf)	number of tapers per frequency. 
!	
!  calls quick, orthog, vvv

!********************************************************************
 
   implicit none

!  Input

   integer, intent(in)    :: nptwo, nf, initap, ntimes
   complex(4), intent(in), dimension(nptwo,2) :: fx
   real(4), intent(in) :: fact, df

!  Out

   integer, intent(out), dimension(nf) :: kopt
   real(4), intent(out), dimension(nf,4) :: sxy

!  Others

   integer :: iter, j, ispan
   real(4) :: c1, c2, d1, d2
   real(4), dimension(nf)   :: y
   real(4), dimension(nf,2) ::  opt

   integer :: ipsd
   real(4) :: R, ak, phi, sigR, optj


!********************************************************************

!  c1, c2=(20*sqrt(1.2))**0.4 are constants for parabolic weighting 
!  in subroutine quick; for uniform weighting c1=1, c2=12.0**0.4=2.702
   
   c1=1.2000 
   c2=3.437

!  Get pilot estimate
   
   call quick2(nptwo,fx,nf,initap,sxy,kopt)

!
!  Estimate K, number of tapers at each freq for MSE spectrum
!  R = S"/S -- use R = Y" + (Y')**2 , Y=ln S.
!  Note  c2=3.437
!

   do iter = 1,ntimes

      do ipsd = 1,2

         do j = 1,nf
            y(j) = log(sxy(j,ipsd))
         enddo

         do j = 1,nf
      
            ispan = kopt(j)*1.4
            call north2(nf, j-ispan, j+ispan, y, d1, d2)

            R  = (d2  + d1**2)/df**2

            ak = kopt(j)/real(2*ispan)
            phi=720.0*ak**5*(1.0 - 1.286*ak + 0.476*ak**3 -   &
                  0.0909*ak**5)
					! From notes (eq 5.16)
			
            sigR= sqrt(phi/real(kopt(j))**5) / df**2
            optj  =c2/(df**4 *( R**2 + 1.4*sigR**2) /fact**2)** 0.2

            opt(j,ipsd) = optj

         enddo
   
      enddo

!  Condition the number of tapers 
      
      call calmer(nf, opt, kopt)
!  Recompute spectrum with optimal variable taper numbers 

      call quick2(nptwo,fx,nf,0,sxy,kopt)

   enddo

   return

end subroutine adapt2

!--------------------------------------------------------------------

subroutine north2(n, i1, i2, s, ds, dds)

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
      
end subroutine north2

!--------------------------------------------------------------------

subroutine curb2(n, v)

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

end subroutine curb2

!--------------------------------------------------------------------

subroutine calmer(nf, opt, kopt)

!
!  calls curb
!  Suppresses strong peaks in Kopt curve that are (almost)
!  always due to S" going through zero.  Restricts rate of
!  growth of weight series to |dK/dn| < 1

!********************************************************************

   implicit none

   integer :: nf
   real(4), dimension(nf,2) :: opt
   integer, dimension(nf)   :: kopt

   integer :: j


!********************************************************************

   do j=1,nf
      opt(j,1)=min(opt(j,1), opt(j,2))
   enddo


   call curb2(nf,opt(1,1))

   do j=1, nf
      kopt(j)=max(opt(j,1),3.0)
   enddo

   return
      
end subroutine calmer

!--------------------------------------------------------------------






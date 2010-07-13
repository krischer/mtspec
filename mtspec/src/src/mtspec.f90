!  MTSPEC SUBROUTINES
!
!  Based on David J. Thomson's codes, Alan Chave and Thomson's Codes and
!  partial codes from EISPACK, Robert L. Parker and Glenn Ierley from
!  Scripps Institution of Oceanography.
!
!  The subroutine is in charge of estimating the adaptive weigthed 
!  multitaper spectrum, as in Thomson 1982. 
!  This is done by estimating the dpss (discrete prolate spheroidal 
!  sequences), multiplying each of the kspec tapers with the data 
!  series, take the fft, and using the adaptive scheme for a better
!  estimation. 
!  As a by product of the spectrum (spec), all intermediate steps 
!  are estimated, and can be called as optional variables (see 
!  documentation). By-products include the complex information
!  in yk, the eigenspectra sk, the jackknife 95% confidence intervals 
!  (err), the degrees of freedom (se) and the weigths wt(nf,kspec) used.
!  To get ahold of this values, simply call them in the subroutine. Note
!  that the order of calling this variables does matter. But if you want 
!  specific variables from the subroutine you can specify them (see 
!  documentation). 
!
!
!  Variable names used in the subroutine
!
!   	xmean	mean of the time series (data is demeaned)
!	xvar	variance of time series
!   	df	frequency bin separation 
!	fnyq	Nyquist frequency
!	sk	eigenspectra (yk**2)
!   	yk 	eigencoefficients
!	sbar	mean spectrum
!	se 	number of degrees of freedom for each frequency bin
!	err	jackknife 95% confidence interval
!	wt	array containing the ne weights for kspec eigenspectra 
!		normalized so that the sum of squares over
!           	the kspec eigenspectra is one
!	lambda  eigenvalues of the dpss
!	theta	1 - eigenvalues
!	vn	dpss sequences
!	xi 	Variance efficiency Eq. 7.2(c)
!	seavg	Stability of estimate (should be close to unity) Eq. 5.5
!	fstat,F	F statistics for single line

!
!
!  Input
!
!	npts	-	integer number of data points in x
!	dt	- 	real, sampling rate of the time series x
!	x(npts) -	real, data vector
!	tbp	-	real, the time-bandwidth product (NW)
!	kspec	- 	integer, number of tapers to use in the estimation
!	nf	-	integer, number of frequency bins (npts/2 + 1)
!
!  Output
!
!	freq(nf)	real vector with frequency bins
!	spec(nf)	real vector with the adaptive estimated spectrum
!
!  Optional Input
!
!	verb		verbose option, print various intermediate 
!			results (y, n)
!	qispec		Use of the QI theory method. 
!			values (0 - normal 1 - QI method)
!			default		normal method.
!       adapt		Use of adaptive or constant weighting
!			values (0 - adaptive 1 - constant weight)
!			default		adaptive 
!       rshape		Perform F-test for lines, and reshape spectrum.
!			If rshape=1, then don't put the lines back 
!			(but keep units correct) 
! 	fcrit   	The threshold probability for the F test
!	nodemean	Use to avoid demeaning time series before 
!			calculating spectra.
!
!  Optional Output
!
!	sk	eigenspectra (yk**2)
!   	yk 	eigencoefficients
!	se 	number of degrees of freedom for each frequency bin
!	err	jackknife 95% confidence interval
!	wt	array containing the ne weights for kspec eigenspectra 
!		normalized so that the sum of squares over
!           	the kspec eigenspectra is one
!	fstat	F statistics for single line

!
!  Modified
!
!	German Prieto
!	April 2005
!       
!       *************************************************************
!
!	Feb 11 / 2007
!	The Quadratic algorithm is now also available. (February 2007)
!
!	In this code, I save three variables npts, kspec and tbp. 
!	This variables have the main information about the tapers 
!	and frequency resolution of the multitaper code, so if this 
!	subroutine is called continuosly by the same main program 
!	it will compute the dpss only if necessary. This part of the
!       code is one of the most time consuming. 
!       
!       *************************************************************
!
!	German A. Prieto
!	September 21, 2007
!	
!	Separated the module spectra.f90 from the main subroutines
!	here. This, to allow additional interfaces to be added, 
!	including sine_psd and subroutines for padded spectra. 
!       
!       *************************************************************
!
!	German A. Prieto
!	June 13, 2008
!
!	Started to work on a multitaper version for complex 
!	variables. This has applications in physical oceanography 
!	and 2D spatial estimation for example for topography, 
!	where the first dimension is real, but then the second 
!	dimension becomes complex. 
!	
!       *************************************************************
!
!	German A. Prieto
!	Oct 7, 2008
!	
!	Added a new feature to not remove the mean inside mtspec.f90
!	if requested. This may be important for auto-coherence, 
!	deconvolutions, etc. 
!
!	*************************************************************
!

!
!  calls
!	dpss, eigenft, adaptspec, jackspec
!	dpss_spline,, noadaptspec,
!	qiinv, 
!	ftest, fdis, psd_reshape
!

!--------------------------------------------------------------------
! Start subroutines
!--------------------------------------------------------------------

subroutine mtspec_d (npts,dt,x,tbp,kspec,nf,freq,spec,             &
		verb,qispec,adapt,                                 &
		yk, wt, err, se, sk,                               &
		rshape, fstat, fcrit, nodemean )

!
!  The double precision version. This is the original version of the 
!  subroutine.
!

!**********************************************************************

   implicit none

!  Input

   integer, intent(in)                  :: npts, nf, kspec
   real(8), intent(in)                  :: dt, tbp
   real(8), intent(in), dimension(npts) :: x

!  Output

   real(8), intent(out), dimension(nf)  :: freq
   real(8), intent(out), dimension(nf)  :: spec

!
!  Optional
!

!  Verbose

   character (len=1),                optional  :: verb
   integer,                          optional  :: qispec, adapt, nodemean
   integer                                     :: v

!  Eigenspectra

   real(8),    dimension(nf,kspec) 	        :: sk_o 
   real(8),    dimension(nf,kspec),   optional  :: sk
   complex(8), dimension(npts,kspec) 	        :: yk_o 
   complex(8), dimension(npts,kspec), optional  :: yk 

!  Simple mean spectra

   real(8), dimension(nf) :: sbar

!  Adaptive spectra

   real(8), dimension(nf) 	          :: se_o 
   real(8), dimension(nf),       optional :: se
   real(8), dimension(nf,kspec)	 	  :: wt_o
   real(8), dimension(nf,kspec), optional :: wt
   real(8), dimension(nf,2) 	          :: err_o
   real(8), dimension(nf,2),     optional :: err
   
!  Efficiency and Stability of estimate

   real(8)	     :: xi, seavg

!  F test, reshaping

   integer,                   optional, intent(in)   :: rshape
   real(4),                   optional, intent(in)   :: fcrit
   real(8),    dimension(nf), optional, intent(out)  :: fstat

   real(8),    dimension(nf)            :: F, sline 

   integer                              :: ier
   real(4),    dimension(nf)	        :: p
   real(4)				:: fcritical
   complex(8), dimension(npts,kspec)    :: yk_shape

!  Quadratic spectrum

   real(8),    dimension(nf)            :: slope

!  Time series

   real(8) :: xmean, xvar 

!  Frequency variables

   real(8) :: df, fnyq

!  Other

   integer                  :: i, j
   real(8)                  :: xsum, sscal
   real(8), dimension(npts) :: x2

! Save values

   integer, save                              :: npts_2, kspec_2
   real(8), save                              :: tbp_2

!  Dpss 

   real(8), dimension(:),   allocatable, save :: lambda, theta
   real(8), dimension(:,:), allocatable, save :: vn

   integer, dimension(1) :: if1, if2

!********************************************************************

   if (present(verb)) then 
      if (index(verb,'n') == 0) then
         v = 1 
      endif
   else
      v = 0
   endif

   spec = 0.d0		! Initialize to zero always
   yk_o  = 0.d0
   sline = 0.d0
   F     = 0.d0

!
! Frequency variables
!

   fnyq	= 0.5d0/dt
   df 	= fnyq/dble(nf - 1)

!
!  Define frequency bins
!

   do i = 1,nf
      freq(i) = dble(i-1)*df
   enddo
 
!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean = sum(x)/dble(npts)
   xvar = (sum((x - xmean)**2))/dble(npts-1)

   if (present(nodemean)) then
      x2 = x 
   else
      x2 = x - xmean 
   endif

   if (all(x2==0.d0)) then
      if (v == 1) then
         write(6,*) 'Vector is a constant, return'
      endif
      return
   endif

!
!  Get the dpss (if already saved, don't compute)
!


   if (kspec/=kspec_2 .or. npts_2/=npts .or. tbp_2/=tbp .or. &
       .not. allocated(vn) ) then
     
      if (allocated(vn)) then
         deallocate(vn, lambda, theta)
      endif
      allocate(vn(npts,kspec))
      allocate(lambda(kspec),theta(kspec))

      if (npts<20000) then
   
         call dpss(npts,tbp,kspec,vn,lambda,theta)

      else

         if (v == 1) then
            write(6,'(a)') 'Computing DPSS with interpolation' 
         endif
         call dpss_spline(10000,npts,tbp,kspec,vn,lambda,theta)
   
      endif
      npts_2 = npts
      kspec_2 = kspec
      tbp_2 = tbp
   endif

   if (v == 1) then 
      write(6,'(3x,a/(4f18.14))')'Prolate spheroidal eigenvalues:',  &
     			       (lambda(j),j=1,kspec)
   endif

!
! Get the eigenspectra (yk's, sk's and smean)
!

   call eigenft(npts, x2, kspec, vn, lambda, nf, yk_o, sk_o, sbar)

!
!  The F test and reshaping
!

   if (present(rshape)) then
      p = 0.0
      call ftest(npts, nf, kspec, vn, yk_o, F)

      ! If requested (rshape=2), only check around 60 Hz
      if (freq(nf) > 60.d0 .and. rshape==2) then 
         if1 = minloc(freq,MASK = freq > (60.d0-tbp/(dble(npts)*dt)) ) 
	 if2 = maxloc(freq,MASK = freq < (60.d0+tbp/(dble(npts)*dt)) )   
        
         do i = if1(1), if2(1)
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo
     
      else 
         do i = 1,nf   
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
            endif
         enddo 
      endif

      if (present(fcrit)) then
         fcritical = fcrit
      else
         fcritical = max(0.95,(real(npts)-5.)/real(npts))
      endif
      if (v == 1) then
         write(6,*) 'Critical F-test value ', fcritical
      endif

      call psd_reshape(npts,nf,kspec,fcritical,p,vn,yk_o,yk_shape,sline)

      do i = 1,nf
         if (sline(i) > 0.0d0) then 
            if (v == 1) then
               write(6,*) 'Line components ', freq(i), real(F(i)), p(i)
            endif
         endif
      enddo

      yk_o = yk_shape	! no more, still need to correct energy

   endif

!
!  Calcuate adaptive weigthed spectrum
!

   if (kspec == 1) then
           wt_o = 1.d0
           se_o = 2.d0
   else
       if (present(adapt)) then
          if (adapt==1) then
             call noadaptspec(npts,nf,kspec,yk_o,spec,se_o,wt_o)
          else
             call adaptspec(npts,nf,kspec,theta,yk_o, &
                          spec,se_o,wt_o)
          endif
       else
          call adaptspec(npts,nf,kspec,theta,yk_o, &
                       spec,se_o,wt_o)
       endif
   endif

!
!  Jackknife error analysis
!

   if (present(err) .and. kspec>1) then
      call jackspec(npts,nf,kspec,theta,yk_o, &
                       spec,se_o,wt_o,err_o)
   else
      err_o = 1.d0
   endif

!  If requested, perform QI inverse theory on top of it. 

   if (present(qispec)) then
      if (qispec==1) then
         call qiinv(npts,tbp,kspec,nf,lambda,vn,yk_o,  &
                    wt_o, spec,slope)   
      endif
   endif

!
!  scale spectrum to meet parseval's theorem
!

   if (present(rshape)) then 
      spec = spec + sline
   endif

!  double power in positive frequencies

   spec(2:nf) = 2.d0 * spec(2:nf)
 
   sscal = (spec(1) + spec(nf))
   do i=2, nf-1
      sscal=sscal + spec(i)
   enddo
   sscal = xvar/(sscal*df)

   if (present(rshape)) then
      if (rshape == 1 .or. rshape == 2) then	! Do not add or save the lines
         spec = spec - 2.d0*sline
      endif
   endif

   spec = sscal*spec

!
!  Put errors on scale
!

   err_o(:,1) = spec / err_o(:,1)
   err_o(:,2) = spec * err_o(:,2)
  
!
!  Average stability (average degrees of freedom)
!		(Eq. 5.5 Pg 1065)
!

   seavg = sum(se_o)/(2.d0*dble(kspec)*dble(nf))
  
   if (v == 1) then 
      write(6,'(a,g15.7)') ' Average stability of estimate = ', seavg
   endif

!
!  compute estimate of variance efficiency 
!		(Eq 7.2(c) Pg 1069)
!
   
   xi=0.d0
   do i=1,npts
      xsum=0.d0
      do j=1,kspec
         xsum = xsum+vn(i,j)**2
      enddo
      xsum = xsum/dble(kspec)
      xi = xi+xsum**2
   enddo
      
   xi=1.d0/(dble(npts)*xi)

   if (v == 1) then
      write(6,'(a,g15.7)') ' Variance efficiency = ', xi
   endif

!
!  Output the appropiate optional variables
!

   if (present(yk)) then
      yk = yk_o
   endif
   if (present(wt)) then
      wt = wt_o
   endif
   if (present(err)) then
      err = err_o
   endif
   if (present(se)) then
      se = se_o
   endif
   if (present(sk)) then
      sk = sk_o
   endif
   if (present(fstat)) then
      fstat = F
   endif

end subroutine mtspec_d

!--------------------------------------------------------------------
! The real*4 version
!--------------------------------------------------------------------

subroutine mtspec_r (npts,dt,x,tbp,kspec,nf,freq,spec,               &
		verb,qispec,adapt,                                   &
		yk, wt, err, se, sk,                                 &
		rshape, fstat, fcrit, nodemean )

!
!  This is the single precision version of the subroutine. It is 
!  simply the original version, with the output changed to real(4).
!

!**********************************************************************

   implicit none

!  Input

   integer, intent(in)                  :: npts, nf, kspec
   real(4), intent(in)                  :: dt, tbp
   real(4), intent(in), dimension(npts) :: x

!  Output

   real(4), intent(out), dimension(nf)  :: freq
   real(4), intent(out), dimension(nf)  :: spec
   real(8), dimension(nf)               :: spec8

!
!  Optional
!

   character (len=1),                optional  :: verb
   integer,                          optional  :: qispec, adapt, nodemean
   integer                                     :: v

!  Eigenspectra

   real(8),    dimension(nf,kspec) 	       :: sk_o 
   real(4),    dimension(nf,kspec),   optional, intent(out) :: sk
   complex(8), dimension(npts,kspec) 	       :: yk_o 
   complex(4), dimension(npts,kspec), optional, intent(out) :: yk 

!  Simple mean spectra

   real(8), dimension(nf) 	    :: sbar

!  Adaptive spectra

   real(8), dimension(nf) 	          :: se_o 
   real(4), dimension(nf),       optional, intent(out) :: se
   real(8), dimension(nf,kspec)	 	  :: wt_o
   real(4), dimension(nf,kspec), optional, intent(out) :: wt
   real(8), dimension(nf,2) 	          :: err_o
   real(4), dimension(nf,2),     optional, intent(out) :: err
   
!  Efficiency and Stability of estimate

   real(4)           :: xi, seavg

!  F test, reshaping

   integer,                   optional, intent(in)   :: rshape
   real(4),                   optional, intent(in)   :: fcrit
   real(4),    dimension(nf), optional, intent(out)  :: fstat

   integer                              :: ier
   real(8),    dimension(nf)            :: F, sline  
   real(4),    dimension(nf)	        :: p
   real(4)				:: fcritical
   complex(8), dimension(npts,kspec)    :: yk_shape

!  Quadratic spectrum

   real(8),    dimension(nf)            :: slope

!  Time series

   real(8) :: xmean, xvar 

!  Frequency variables

   real(8) :: df, fnyq

!  Other

   integer                  :: i, j
   real(4)                  :: xsum
   real(8)                  :: sscal
   real(8), dimension(npts) :: x2

! Save values

   integer, save                              :: npts_2, kspec_2
   real(4), save                              :: tbp_2

!  Dpss 

   real(8), dimension(:),   allocatable, save :: lambda, theta
   real(8), dimension(:,:), allocatable, save :: vn

   integer, dimension(1) :: if1, if2

!********************************************************************

   if (present(verb)) then 
      if (index(verb,'n') == 0) then
         v = 1 
      endif
   else
      v = 0 
   endif

   spec8 = 0.d0		! Initialize to zero always
   yk_o  = 0.d0
   sline = 0.d0
   F     = 0.d0

!
! Frequency variables
!

   fnyq	= 0.5d0/dble(dt)
   df 	= fnyq/dble(nf - 1)

!
!  Define frequency bins
!

   do i = 1,nf
      freq(i) = real(i-1)*real(df)
   enddo
  
!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean = sum(dble(x))/dble(npts)
   xvar = (sum((dble(x) - xmean)**2))/dble(npts-1)

   if (present(nodemean)) then
      if (v == 1) then
         print *, 'No demean'
      endif
      x2 = dble(x) 
   else
      if (v == 1) then
          print *, 'Demeaned'
      endif
      x2 = dble(x) - xmean 
   endif

   if (all(x2==0.d0)) then
      if (v == 1) then
         write(6,*) 'Vector is a constant, return'
      endif
      return
   endif

!
!  Get the dpss (if already saved, don't compute)
!

   if (kspec/=kspec_2 .or. npts_2/=npts .or. tbp_2/=tbp .or. &
       .not. allocated(vn) ) then

      if (allocated(vn)) then
         deallocate(vn, lambda, theta)
      endif
      allocate(vn(npts,kspec))
      allocate(lambda(kspec),theta(kspec))
 
      if (npts<20000) then
   
         call dpss(npts,dble(tbp),kspec,vn,lambda,theta)

      else
      
         if (v == 1) then
            write(6,'(a)') 'Computing DPSS with interpolation' 
         endif
         call dpss_spline(10000,npts,dble(tbp),kspec,vn,lambda,theta)
   
      endif
      npts_2 = npts
      kspec_2 = kspec
      tbp_2 = tbp
   endif

   if (v == 1) then
      write(6,'(3x,a/(4f18.14))')'Prolate spheroidal eigenvalues:',  &
      			       (lambda(j),j=1,kspec)
   endif

!
! Get the eigenspectra (yk's, sk's and smean)
!

   call eigenft(npts, x2, kspec, vn, lambda, nf, yk_o, sk_o, sbar)

!
!  The F test and reshaping
!

   if (present(rshape)) then
      p = 0.0
      call ftest(npts, nf, kspec, vn, yk_o, F)

      ! If requested (rshape=2), only check around 60 Hz.
      if (freq(nf) > 60. .and. rshape==2) then 
         if1 = minloc(freq,MASK = freq > (60.-tbp/(real(npts)*dt)) ) 
	 if2 = maxloc(freq,MASK = freq < (60.+tbp/(real(npts)*dt)) )   
      
         do i = if1(1), if2(1)
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo

      else
         do i = 1,nf   
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo 
      endif

      if (present(fcrit)) then
         fcritical = fcrit
      else
         fcritical = max(0.95,(real(npts)-5.)/real(npts))
      endif
      if (v == 1) then 
         write(6,*) 'Critical F-test value ', fcritical
      endif

      call psd_reshape(npts,nf,kspec,fcritical,p,vn,yk_o,yk_shape,sline)

      do i = 1,nf
         if (sline(i) > 0.0d0) then 
            if (v == 1) then
               write(6,*) 'Line components ', freq(i), real(F(i)), p(i)
            endif
         endif
      enddo

      yk_o = yk_shape	! no more, still need to correct energy

   endif

!
!  Calculate adaptive weigthed spectrum, or constant weighting if asked.
!

   if (present(adapt)) then
      if (adapt==1) then
         call noadaptspec(npts,nf,kspec,yk_o,spec8,se_o,wt_o)
      else
         call adaptspec(npts,nf,kspec,theta,yk_o, &
                      spec8,se_o,wt_o)
      endif
   else
      call adaptspec(npts,nf,kspec,theta,yk_o, &
                   spec8,se_o,wt_o)
   endif

   if (present(err) .and. kspec>1) then
      call jackspec(npts,nf,kspec,theta,yk_o, &
                       spec8,se_o,wt_o,err_o)
   else
      err_o = 1.d0
   endif

!  If requested, perform QI inverse theory on top of it. 

   if (present(qispec)) then 
      if (qispec==1) then
         call qiinv(npts,dble(tbp),kspec,nf,lambda,vn,yk_o,  &
                    wt_o,spec8,slope) 
      endif  
   endif

!
!  scale spectrum to meet parseval's theorem
!

   if (present(rshape)) then 
      spec8 = spec8 + sline
   endif

!  double power in positive frequencies

   spec8(2:nf) = 2.d0 * spec8(2:nf)
 
   sscal = (spec8(1) + spec8(nf))
 
   do i=2, nf-1
      sscal=sscal + spec8(i)
   enddo
   sscal = xvar/(sscal*df)

   if (present(rshape)) then
      if (rshape == 1 .or. rshape == 2) then	! Do not add or save the lines
         spec8 = spec8 - 2.d0*sline
      endif
   endif

   spec = real(sscal*spec8)

!
!  Put errors on scale
!

   err_o(:,1) = dble(spec) / err_o(:,1)
   err_o(:,2) = dble(spec) * err_o(:,2)
 
!
!  Average stability (average degrees of freedom)
!		(Eq. 5.5 Pg 1065)
!

   seavg = real(sum(se_o))/(2.*real(kspec)*real(nf))
  
   if (v == 1) then 
      write(6,'(a,g15.7)') ' Average stability of estimate = ', seavg
   endif
!
!  compute estimate of variance efficiency 
!		(Eq 7.2(c) Pg 1069)
!
   
   xi = 0.
   do i=1,npts
      xsum=0.
      do j=1,kspec
         xsum = xsum + real(vn(i,j)**2)
      enddo
      xsum = xsum/real(kspec)
      xi   = xi + xsum**2
   enddo
      
   xi = 1./(real(npts)*xi)

   if (v == 1) then 
      write(6,'(a,g15.7)') ' Variance efficiency = ', xi
   endif
!
!  Output the appropiate optional variables
!

   if (present(yk)) then
      yk = cmplx(yk_o)
   endif
   if (present(wt)) then
      wt = real(wt_o)
   endif
   if (present(err)) then
      err = real(err_o)
   endif
   if (present(se)) then
      se = real(se_o)
   endif
   if (present(sk)) then
      sk = real(sk_o)
   endif
   if (present(fstat)) then
      fstat = real(F)
   endif

   return

end subroutine mtspec_r

!--------------------------------------------------------------------
! The real*4 matrix version
!--------------------------------------------------------------------


subroutine mtspec_m (ntimes,npts,dt,x,tbp,kspec,nf,freq,spec,err, &
			nodemean )

!
!  This is the single precision matrix version of the subroutine. It is 
!  simply the original version, with the output changed to real(4), in 
!  matrix form
!

!**********************************************************************

   implicit none

!  Input

   integer, intent(in) :: npts, nf, kspec,ntimes
   real(4), intent(in) :: dt, tbp
   real(4), intent(in), dimension(npts,ntimes) :: x

!  Output

   real(4), intent(out), dimension(nf) :: freq
   real(4), intent(out), dimension(nf,ntimes) :: spec
   real(8), dimension(nf) :: spec8
   real(4), intent(out), dimension(nf,2,ntimes) :: err
   real(8), dimension(nf,2) 	      :: err8

!
!  Optional
!

   integer, optional       :: nodemean

!  Eigenspectra

   real(8), dimension(nf,kspec) 	       :: sk 
   complex(8), dimension(npts,kspec) 	       :: yk 

!  Simple mean spectra

   real(8), dimension(nf) 	    :: sbar

!  Adaptive spectra

   real(8), dimension(nf) 	          :: se 
   real(8), dimension(nf,kspec)	 	  :: wt

!  Dpss 

   real(8), dimension(kspec)	       :: lambda, theta
   real(8), dimension(npts,kspec)      :: vn

!  Efficiency and Stability of estimate

   real(4)	     :: xi, seavg

!
!  Extras
!

!  Time series

   real(8) :: xmean, xvar 

!  Frequency variables

   real(8) :: df, fnyq

!  Other

   integer :: i, j, k
   real(4) :: xsum
   real(8) :: sscal
   real(8), dimension(npts) :: x2

!********************************************************************

   write(6,'(a)') 'Calling matrix single precision'

!
! Frequency variables
!

   fnyq	= 0.5d0/dble(dt)
   df 	= fnyq/dble(nf - 1)

!
!  Define frequency bins
!

   do i = 1,nf
      freq(i) = real(i-1)*real(df)
   enddo

!
!  Get the dpss
!

   if (npts<20000) then
   
      call dpss(npts,dble(tbp),kspec,vn,lambda,theta)

   else

      write(6,'(a)') 'Computing DPSS with interpolation' 
      call dpss_spline(10000,npts,dble(tbp),kspec,vn,lambda,theta)
   
   endif

   write(6,'(3x,a/(4f18.14))')'Prolate spheroidal eigenvalues:',  &
     			       (lambda(j),j=1,kspec)


   do k = 1,ntimes

   !
   !  Get time series stats (mean and variance)
   !  Always demean the time series
   !

      xmean = sum(dble(x(:,k)))/dble(npts)
      xvar = (sum((dble(x(:,k)) - xmean)**2))/dble(npts-1)

      if (present(nodemean)) then
         x2 = dble(x(:,k)) 
      else
         x2 = dble(x(:,k)) - xmean 
      endif

   !
   ! Get the eigenspectra (yk's, sk's and smean)
   !

      call eigenft(npts, x2, kspec, vn, lambda, nf, yk, sk, sbar)


   !
   !  Calcuate adaptive weigthed spectrum
   !

      call adaptspec(npts,nf,kspec,theta,yk, &
                   spec8,se,wt)

      if (kspec>1) then
         call jackspec(npts,nf,kspec,theta,yk, &
                      spec8,se,wt,err8)
      else
         err8 = 1.d0
      endif


   !
   !  scale spectrum to meet parseval's theorem
   !

   !  double power in positive frequencies

      spec8(2:nf) = 2.d0 * spec8(2:nf)
 
      sscal = (spec8(1) + spec8(nf))

      do i=2, nf-1
         sscal=sscal + spec8(i)
      enddo
      sscal = xvar/(sscal*df)

      spec(:,k) = real(sscal*spec8)

   !
   !  Put errors on scale
   !

      err8(:,1) = dble(spec(:,k)) / err8(:,1) 
      err8(:,2) = dble(spec(:,k)) * err8(:,2) 
  
      err(:,1,k) = real(err8(:,1))
      err(:,2,k) = real(err8(:,2))

   !
   !  Average stability (average degrees of freedom)
   !		(Eq. 5.5 Pg 1065)
   !

      seavg = real(sum(se))/(2.*real(kspec)*real(nf))
   
      write(6,'(a,g15.7)') ' Average stability of estimate = ', seavg

   !
   !  compute estimate of variance efficiency 
   !		(Eq 7.2(c) Pg 1069)
   !
   
      xi=0.
      do i=1,npts
         xsum=0.
         do j=1,kspec
            xsum = xsum+real(vn(i,j)**2)
         enddo
         xsum = xsum/real(kspec)
         xi = xi + xsum**2
      enddo
      
      xi = 1./(real(npts)*xi)
  
      write(6,'(a,g15.7)') ' Variance efficiency = ', xi

   enddo

end subroutine mtspec_m

!--------------------------------------------------------------------
! The real*4 version with zero padding
!--------------------------------------------------------------------


subroutine mtspec_pad (npts,nfft,dt,x,tbp,kspec,nf,freq,spec,           &
		verb,qispec,adapt,                                      &
		yk, wt, err, se, sk,                                    &
		rshape, fstat, fcrit, nodemean )

!
!  This is the single precision version of the subroutine. It is 
!  simply the original version, with the output changed to real(4), 
!  and the tapered series are padded to NFFT points. 
!

!**********************************************************************

   implicit none

!  Input

   integer, intent(in)                  :: npts, nfft, nf, kspec
   real(4), intent(in)                  :: dt, tbp
   real(4), intent(in), dimension(npts) :: x

!  Output

   real(4), intent(out), dimension(nf)  :: freq
   real(4), intent(out), dimension(nf)  :: spec
   real(8),              dimension(nf)  :: spec8

!
!  Optional
!

   character (len=1),                optional  :: verb
   integer,                          optional  :: qispec, adapt, nodemean
   integer                                     :: v

!  Eigenspectra

   real(8),    dimension(nf,kspec) 	       :: sk_o 
   real(4),    dimension(nf,kspec),   optional, intent(out) :: sk
   complex(8), dimension(nfft,kspec) 	       :: yk_o 
   complex(4), dimension(nfft,kspec), optional, intent(out) :: yk 

!  Quadratic spectrum

   real(8),    dimension(nf)            :: slope

!  Simple mean spectra

   real(8), dimension(nf) 	    :: sbar

!  Adaptive spectra

   real(8), dimension(nf) 	          :: se_o 
   real(4), dimension(nf),       optional, intent(out) :: se
   real(8), dimension(nf,kspec)	 	  :: wt_o
   real(4), dimension(nf,kspec), optional, intent(out) :: wt
   real(8), dimension(nf,2) 	          :: err_o
   real(4), dimension(nf,2),     optional, intent(out) :: err
   
!  Efficiency and Stability of estimate

   real(4) :: xi, seavg

!  F test, reshaping

   integer,                   optional, intent(in)   :: rshape
   real(4),                   optional, intent(in)   :: fcrit
   real(4),    dimension(nf), optional, intent(out)  :: fstat

   integer                              :: ier
   real(8),    dimension(nf)            :: F, sline  
   real(4),    dimension(nf)	        :: p
   real(4)				:: fcritical
   complex(8), dimension(nfft,kspec)    :: yk_shape

!
!  Extras
!

!  Time series

   real(8) :: xmean, xvar 

!  Frequency variables

   real(8) :: df, fnyq

!  Other

   integer                  :: i, j
   real(4)                  :: xsum
   real(8)                  :: sscal
   real(8), dimension(npts) :: x2

! Save values

   integer, save                              :: npts_2, kspec_2
   real(4), save                              :: tbp_2

!  Dpss 
!
   real(8), dimension(:),   allocatable, save :: lambda, theta
   real(8), dimension(:,:), allocatable, save :: vn

   integer, dimension(1) :: if1, if2

!********************************************************************

   if (present(verb)) then 
      if (index(verb,'n') == 0) then
         v = 1 
      endif
   else
      v = 0 
   endif

   if (npts > nfft) then 
      write(6,'(a,2i10)') 'For zero padding nfft > npts', nfft, npts
      stop
   endif

   if (nf /= nfft/2+1) then
      write(6,'(a)') 'nf is spectrum has to be nfft/2+1 for zero padding'
      stop
   endif

   spec8 = 0.d0		! Initialize to zero always
   yk_o  = 0.d0
   sline = 0.d0
   F     = 0.d0

!
! Frequency variables
!

   fnyq	= 0.5d0/dble(dt)
   df 	= fnyq/dble(nf - 1)

!
!  Define frequency bins
!

   do i = 1,nf
      freq(i) = real(i-1)*real(df)
   enddo

!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean = sum(dble(x))/dble(npts)
   xvar = (sum((dble(x) - xmean)**2))/dble(npts-1)

   if (present(nodemean)) then
      x2 = dble(x) 
   else
      x2 = dble(x) - xmean 
   endif

   if (all(x2==0.d0)) then
      if (v == 1) then
         write(6,*) 'Vector is a constant, return'
      endif
      return
   endif

!
!  Get the dpss (if already saved, don't compute)
!

   if (kspec/=kspec_2 .or. npts_2/=npts .or. tbp_2/=tbp .or. &
       .not. allocated(vn) ) then

      if (allocated(vn)) then
         deallocate(vn, lambda, theta)
      endif
      allocate(vn(npts,kspec))
      allocate(lambda(kspec),theta(kspec))
 
      if (npts<20000) then
   
         call dpss(npts,dble(tbp),kspec,vn,lambda,theta)

      else
      
         if (v == 1) then
            write(6,'(a)') 'Computing DPSS with interpolation' 
         endif
         call dpss_spline(10000,npts,dble(tbp),kspec,vn,lambda,theta)
   
      endif
      npts_2 = npts
      kspec_2 = kspec
      tbp_2 = tbp
   endif

   if (v == 1) then
      write(6,'(3x,a/(4f18.14))')'Prolate spheroidal eigenvalues:',  &
      			       (lambda(j),j=1,kspec)
   endif

!
! Get the eigenspectra (yk's, sk's and smean)
!

   call eigenft_pad(npts, nfft, x2, kspec, vn, lambda, nf, yk_o, sk_o, sbar)

!
!  The F test and reshaping
!

   if (present(rshape)) then
      p = 0.0
      call ftest_pad(npts, nfft, nf, kspec, vn, yk_o, F)

      ! If requested (rshape=2), only check around 60 Hz.
      if (freq(nf) > 60. .and. rshape==2) then 
         if1 = minloc(freq,MASK = freq > (60.-tbp/(real(npts)*dt)) ) 
	 if2 = maxloc(freq,MASK = freq < (60.+tbp/(real(npts)*dt)) )   
      
         do i = if1(1), if2(1)
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo

      else
         do i = 1,nf   
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo 
      endif

      if (present(fcrit)) then
         fcritical = fcrit
      else
         fcritical = max(0.95,(real(npts)-5.)/real(npts))
      endif
      if (v == 1) then 
         write(6,*) 'Critical F-test value ', fcritical
      endif

      call psd_reshape_pad(npts,nfft,nf,kspec,fcritical,p,vn,yk_o,yk_shape,sline)

      do i = 1,nf
         if (sline(i) > 0.0d0) then 
            if (v == 1) then
               write(6,*) 'Line components ', freq(i), real(F(i)), p(i)
            endif
         endif
      enddo

      yk_o = yk_shape	! no more, still need to correct energy
      
   endif

!
!  Calculate adaptive weigthed spectrum, or constant weighting if asked.
!

   if (present(adapt)) then
      if (adapt==1) then
         call noadaptspec(nfft,nf,kspec,yk_o,spec8,se_o,wt_o)
      else
         call adaptspec(nfft,nf,kspec,theta,yk_o, &
                      spec8,se_o,wt_o)
      endif
   else
      call adaptspec(nfft,nf,kspec,theta,yk_o, &
                   spec8,se_o,wt_o)
   endif

   if (present(err) .and. kspec>1) then
      call jackspec(nfft,nf,kspec,theta,yk_o, &
                       spec8,se_o,wt_o,err_o)
   else
      err_o = 1.d0
   endif


!  If requested, perform QI inverse theory on top of it. 

   if (present(qispec)) then 
      if (qispec==1) then
         write(6,'(a)') 'No quadratic multitaper supported yet'
         !call qiinv(nfft,dble(tbp),kspec,nf,lambda,vn,yk_o,  &
         !           wt_o,spec8,slope) 
      endif  
   endif

!
!  scale spectrum to meet parseval's theorem
!

   if (present(rshape)) then 
      spec8 = spec8 + sline
   endif

!  double power in positive frequencies

   spec8(2:nf) = 2.d0 * spec8(2:nf)
 
   sscal = (spec8(1) + spec8(nf))
 
   do i=2, nf-1
      sscal=sscal + spec8(i)
   enddo
   sscal = xvar/(sscal*df)

   if (present(rshape)) then
      if (rshape == 1 .or. rshape == 2) then	! Do not add or save the lines
         spec8 = spec8 - 2.d0*sline
      endif
   endif

   spec = real(sscal*spec8)

!
!  Put errors on scale
!

   err_o(:,1) = dble(spec) / err_o(:,1)
   err_o(:,2) = dble(spec) * err_o(:,2)
 
!
!  Average stability (average degrees of freedom)
!		(Eq. 5.5 Pg 1065)
!

   seavg = real(sum(se_o))/(2.*real(kspec)*real(nf))
  
   if (v == 1) then 
      write(6,'(a,g15.7)') ' Average stability of estimate = ', seavg
   endif
!
!  compute estimate of variance efficiency 
!		(Eq 7.2(c) Pg 1069)
!
   
   xi=0.
   do i=1,npts
      xsum=0.
      do j=1,kspec
         xsum = xsum +real(vn(i,j)**2)
      enddo
      xsum = xsum/real(kspec)
      xi = xi + xsum**2
   enddo
      
   xi = 1./(real(npts)*xi)

   if (v == 1) then 
      write(6,'(a,g15.7)') ' Variance efficiency = ', xi
   endif
!
!  Output the appropiate optional variables
!

   if (present(yk)) then
      yk = cmplx(yk_o)
   endif
   if (present(wt)) then
      wt = real(wt_o)
   endif
   if (present(err)) then
      err = real(err_o)
   endif
   if (present(se)) then
      se = real(se_o)
   endif
   if (present(sk)) then
      sk = real(sk_o)
   endif
   if (present(fstat)) then
      fstat = real(F)
   endif

   return

end subroutine mtspec_pad

!--------------------------------------------------------------------
! The complex*4 version
!--------------------------------------------------------------------

subroutine mtspec_c (npts,dt,x,tbp,kspec,nf,freq,spec,               &
		verb,qispec,adapt,                                   &
		yk, wt, err, se, sk,                                 &
		rshape, fstat, fcrit, nodemean )

!
!  This is the single precision version of the subroutine. It is 
!  simply the original version, with the output changed to real(4).
!

!**********************************************************************

   implicit none

!  Input

   integer,    intent(in)                  :: npts, nf, kspec
   real(4),    intent(in)                  :: dt, tbp
   complex(4), intent(in), dimension(npts) :: x

!  Output

   real(4), intent(out), dimension(nf)  :: freq
   real(4), intent(out), dimension(nf)  :: spec
   real(8), dimension(nf)               :: spec8

!
!  Optional
!

   character (len=1),                optional  :: verb
   integer,                          optional  :: qispec, adapt, nodemean
   integer                                     :: v

!  Eigenspectra

   real(8),    dimension(nf,kspec) 	       :: sk_o 
   real(4),    dimension(nf,kspec),   optional, intent(out) :: sk
   complex(8), dimension(npts,kspec) 	       :: yk_o 
   complex(4), dimension(npts,kspec), optional, intent(out) :: yk 

!  Simple mean spectra

   real(8), dimension(nf) 	    :: sbar

!  Adaptive spectra

   real(8), dimension(nf) 	          :: se_o 
   real(4), dimension(nf),       optional, intent(out) :: se
   real(8), dimension(nf,kspec)	 	  :: wt_o
   real(4), dimension(nf,kspec), optional, intent(out) :: wt
   real(8), dimension(nf,2) 	          :: err_o
   real(4), dimension(nf,2),     optional, intent(out) :: err
   
!  Efficiency and Stability of estimate

   real(4)           :: xi, seavg

!  F test, reshaping

   integer,                   optional, intent(in)   :: rshape
   real(4),                   optional, intent(in)   :: fcrit
   real(4),    dimension(nf), optional, intent(out)  :: fstat

   integer                              :: ier
   real(8),    dimension(nf)            :: F, sline  
   real(4),    dimension(nf)	        :: p
   real(4)				:: fcritical
   complex(8), dimension(npts,kspec)    :: yk_shape

!  Quadratic spectrum

   real(8),    dimension(nf)            :: slope

!  Time series

   complex(8) :: xmean
   real(8)    :: xvar 

!  Frequency variables

   real(8) :: df, fnyq

!  Other

   integer                  :: i, j, nf2
   real(4)                  :: xsum
   real(8)                  :: sscal
   complex(8), dimension(npts) :: x2

! Save values

   integer, save                              :: npts_2, kspec_2
   real(4), save                              :: tbp_2

!  Dpss 

   real(8), dimension(:),   allocatable, save :: lambda, theta
   real(8), dimension(:,:), allocatable, save :: vn

   integer, dimension(1) :: if1, if2

!********************************************************************

   if (nf < npts) then
      write(6,*) 'For complex variables, nf == npts', nf, npts
      stop
   endif

   if (present(verb)) then 
      if (index(verb,'n') == 0) then
         v = 1 
      endif
   else
      v = 0 
   endif

   spec8 = 0.d0		! Initialize to zero always
   yk_o  = 0.d0
   sline = 0.d0
   F     = 0.d0

!
! Frequency variables
!

   if (mod(npts,2)==0) then
      nf2 = npts/2 + 1;
   else
      nf2 = (npts+1)/2;
   endif

   fnyq	= 0.5d0/dble(dt)
   df 	= fnyq/dble(nf2-1)

!
!  Define frequency bins
!

   freq = real( (/(i-1, i=1,nf2), (i, i=-nf2+2,1)/) ) * df

!
!  Get time series stats (mean and variance)
!  Always demean the time series
!

   xmean = sum(cmplx(x,kind=8))/dble(npts)
   xvar = (sum((cmplx(x,kind=8) - xmean)**2))/dble(npts-1)

   if (present(nodemean)) then
      x2 = cmplx(x,kind=8) 
   else
      x2 = cmplx(x,kind=8) - xmean 
   endif

   if (all(x2==0.d0)) then
      if (v == 1) then
         write(6,*) 'Vector is a constant, return'
      endif
      return
   endif

!
!  Get the dpss (if already saved, don't compute)
!

   if (kspec/=kspec_2 .or. npts_2/=npts .or. tbp_2/=tbp .or. &
       .not. allocated(vn) ) then

      if (allocated(vn)) then
         deallocate(vn, lambda, theta)
      endif
      allocate(vn(npts,kspec))
      allocate(lambda(kspec),theta(kspec))
 
      if (npts<20000) then
   
         call dpss(npts,dble(tbp),kspec,vn,lambda,theta)

      else
      
         if (v == 1) then
            write(6,'(a)') 'Computing DPSS with interpolation' 
         endif
         call dpss_spline(10000,npts,dble(tbp),kspec,vn,lambda,theta)
   
      endif
      npts_2 = npts
      kspec_2 = kspec
      tbp_2 = tbp
   endif

   if (v == 1) then
      write(6,'(3x,a/(4f18.14))')'Prolate spheroidal eigenvalues:',  &
      			       (lambda(j),j=1,kspec)
   endif

!
! Get the eigenspectra (yk's, sk's and smean)
!

   call eigenft(npts, x2, kspec, vn, lambda, nf, yk_o, sk_o, sbar)

!
!  The F test and reshaping
!

   if (present(rshape)) then
      p = 0.0
      call ftest(npts, nf, kspec, vn, yk_o, F)

      ! If requested (rshape=2), only check around 60 Hz.
      if (freq(nf) > 60. .and. rshape==2) then 
         if1 = minloc(freq,MASK = freq > (60.-tbp/(real(npts)*dt)) ) 
	 if2 = maxloc(freq,MASK = freq < (60.+tbp/(real(npts)*dt)) )   
      
         do i = if1(1), if2(1)
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo

      else
         do i = 1,nf   
            call fdis(real(F(i)),2,2*(kspec-1),p(i),ier)
            if (ier /= 0) then
               write(6,*) 'Error in F distribution ', ier
               stop
            endif
         enddo 
      endif

      if (present(fcrit)) then
         fcritical = fcrit
      else
         fcritical = max(0.95,(real(npts)-5.)/real(npts))
      endif
      if (v == 1) then 
         write(6,*) 'Critical F-test value ', fcritical
      endif

      call psd_reshape(npts,nf,kspec,fcritical,p,vn,yk_o,yk_shape,sline)

      do i = 1,nf
         if (sline(i) > 0.0d0) then 
            if (v == 1) then
               write(6,*) 'Line components ', freq(i), real(F(i)), p(i)
            endif
         endif
      enddo

      yk_o = yk_shape	! no more, still need to correct energy

   endif

!
!  Calculate adaptive weigthed spectrum, or constant weighting if asked.
!

   if (present(adapt)) then
      if (adapt==1) then
         call noadaptspec(npts,nf,kspec,yk_o,spec8,se_o,wt_o)
      else
         call adaptspec(npts,nf,kspec,theta,yk_o, &
                      spec8,se_o,wt_o)
      endif
   else
      call adaptspec(npts,nf,kspec,theta,yk_o, &
                   spec8,se_o,wt_o)
   endif

   if (present(err) .and. kspec>1) then
      call jackspec(npts,nf,kspec,theta,yk_o, &
                       spec8,se_o,wt_o,err_o)
   else
      err_o = 1.d0
   endif

!  If requested, perform QI inverse theory on top of it. 

   if (present(qispec)) then 
      if (qispec==1) then
         call qiinv(npts,dble(tbp),kspec,nf,lambda,vn,yk_o,  &
                    wt_o,spec8,slope) 
      endif  
   endif

!
!  scale spectrum to meet parseval's theorem
!

   if (present(rshape)) then 
      spec8 = spec8 + sline
   endif

!  double power in positive frequencies

   sscal = 0.0d0
 
   do i=1, nf
      sscal=sscal + spec8(i)
   enddo
   sscal = xvar/(sscal*df)

   if (present(rshape)) then
      if (rshape == 1 .or. rshape == 2) then	! Do not add or save the lines
         spec8 = spec8 - 2.d0*sline
      endif
   endif

   spec = real(sscal*spec8)

!
!  Put errors on scale
!

   err_o(:,1) = dble(spec) / err_o(:,1)
   err_o(:,2) = dble(spec) * err_o(:,2)
 
!
!  Average stability (average degrees of freedom)
!		(Eq. 5.5 Pg 1065)
!

   seavg = real(sum(se_o))/(2.*real(kspec)*real(nf))
  
   if (v == 1) then 
      write(6,'(a,g15.7)') ' Average stability of estimate = ', seavg
   endif
!
!  compute estimate of variance efficiency 
!		(Eq 7.2(c) Pg 1069)
!
   
   xi = 0.
   do i=1,npts
      xsum=0.
      do j=1,kspec
         xsum = xsum + real(vn(i,j)**2)
      enddo
      xsum = xsum/real(kspec)
      xi   = xi + xsum**2
   enddo
      
   xi = 1./(real(npts)*xi)

   if (v == 1) then 
      write(6,'(a,g15.7)') ' Variance efficiency = ', xi
   endif
!
!  Output the appropiate optional variables
!

   if (present(yk)) then
      yk = cmplx(yk_o)
   endif
   if (present(wt)) then
      wt = real(wt_o)
   endif
   if (present(err)) then
      err = real(err_o)
   endif
   if (present(se)) then
      se = real(se_o)
   endif
   if (present(sk)) then
      sk = real(sk_o)
   endif
   if (present(fstat)) then
      fstat = real(F)
   endif

   return

end subroutine mtspec_c


!--------------------------------------------------------------------
! End subroutines
!--------------------------------------------------------------------







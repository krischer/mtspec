subroutine mt_transfer(npts,nfft,dt,xi,xj,tbp,kspec,nf,    &
                       freq,cohe,trf,cspec,speci,specj,iadapt, &
			demean,nodemean)

!
!  subroutine to compute transfer function and squared multiple
!  coherence between one input and one output time series using
!  the multitaper method by Thomson.
!  Jackknife estimates for 95% confidence limits will also be 
!  made available in the future. 
!
!  INPUT
!
!	npts		integer number of points in time series
!       nfft		integer number of point to pad
! 	dt		real, sampling rate of time series
!	xi(npts)	real, data for first series
!       xj(npts) 	real, data for second series
!	tbp		the time-bandwidth product
!	kspec		integer, number of tapers to use
!	nf		integer, number of freq points in spectrum
!
!
!  OPTIONAL INPUT
!
!	iadapt		integer 0 - adaptive, 1 - constant weights
!			default adapt = 1
!	demean	        if present, force complex TF to be demeaned.
!	nodemean	if present, time series are not demeaned 
!			before TF calculations. 
!	
!  OPTIONAL OUTPUT
!
!	freq(nf)	real vector with frequency bins
!       cohe(nf)	real, coherence of the two series (0 - 1)
!       trf(nfft)       complex array with the transfer function
!	cspec(nf)	complex cross-spectrum estimate
!	speci(nf)	real, PSD of first signal
!	specj(nf)	real, PSD of second signal
!
!  Modified 
!
!	German Prieto
!	October 2007
!
!	August 2008
!		Auto-transfer is now applicable
!
!	October 2008
!		Time series don't have to be demeaned before TF calculation
!
!	*******************************************************************

!
!  calls mtspec
!        sym_fft (if trf is requested)

!********************************************************************

   use spectra

   implicit none

!  Inputs

   integer, intent(in)                      :: npts, kspec
   integer, intent(in)                      :: nf, nfft

   real(4), intent(in)                      :: dt, tbp

   real(4), dimension(npts), intent(in)     :: xi, xj

!  Optional Input

   integer, optional                        :: iadapt, demean, nodemean

!  Outputs

   real(4),    dimension(nf),   optional    :: freq
   real(4),    dimension(nf),   optional    :: cohe
   complex(4), dimension(nf),   optional    :: cspec
   complex(4), dimension(nfft), optional    :: trf
   real(4),    dimension(nf),   optional    :: speci, specj

! Working variables

   real(4), dimension(nf)            :: f, si, sj
   real(4), dimension(nf)	     :: coh

   real(4), dimension(nf,kspec)      :: wt_i, wt_j 
   complex(4), dimension(nfft,kspec) :: yk_i, yk_j
   complex(4), dimension(nf)         :: xspec, csp

   real(4), dimension(nf)            :: wt_scale
   complex(4), dimension(nf,kspec)   :: dyk_i, dyk_j 

   integer                           :: i, j, iad

!********************************************************************

!   write(6,'(a)') 'Calling multitaper transfer function code'
!   write(6,'(a,3i8)') 'Data and frequency points', npts, nfft, nf

   if (present(iadapt)) then 
      if (iadapt == 0) then
         iad = 0
      else
         iad = 1
      endif
   else
      iad = 1
   endif
   
!
!  Get the spectrum estimate
!

   if (present(nodemean)) then
      call mtspec( npts,nfft,dt,xi,tbp,kspec,nf,f,          &
                    si,yk=yk_i,wt=wt_i,adapt=iad,nodemean=1)
      call mtspec( npts,nfft,dt,xj,tbp,kspec,nf,f,          &
    		    sj,yk=yk_j,wt=wt_j,adapt=iad,nodemean=1)
   else
      call mtspec( npts,nfft,dt,xi,tbp,kspec,nf,f,          &
                    si,yk=yk_i,wt=wt_i,adapt=iad)

      call mtspec( npts,nfft,dt,xj,tbp,kspec,nf,f,          &
    		    sj,yk=yk_j,wt=wt_j,adapt=iad)
   endif

!
!  Create the spectra (cannot use spec output, normalized different)
!

   wt_i = min(wt_i,wt_j)
   wt_j = min(wt_i,wt_j)

   wt_scale = sum(wt_i**2, dim=2)  ! Scale weights to keep power 
   do i = 1,kspec
      wt_i(:,i) = wt_i(:,i)/sqrt(wt_scale)
      wt_j(:,i) = wt_j(:,i)/sqrt(wt_scale)
   enddo

   do i = 1,nf
      do j = 1,kspec
         dyk_i(i,j) = wt_i(i,j) * yk_i(i,j)
         dyk_j(i,j) = wt_j(i,j) * yk_j(i,j)
      enddo
   enddo

!  Force a zero mean process

   do i = 1,kspec
      dyk_i(:,i) = dyk_i(:,i) - sum(real(dyk_i(:,i)))/real(nfft)
      dyk_j(:,i) = dyk_j(:,i) - sum(real(dyk_j(:,i)))/real(nfft)
   enddo

   si = sum(abs(dyk_i)**2, dim=2) 
   sj = sum(abs(dyk_j)**2, dim=2) 

   do i = 1,nf

      csp(i) = sum ( dyk_i(i,:) * conjg(dyk_j(i,:)) )  

   ! coherence 

      coh(i) = (abs(csp(i)))**2 / (si(i)*sj(i))
      
   ! Transfer function

      xspec(i) = csp(i) / sj(i)

   enddo

   if (present(demean)) then
      xspec = xspec - sum(real(xspec))/real(nf)   !  Force a zero mean
   endif
   if (present(cohe)) then
      cohe = coh
   endif
   if (present(cspec)) then
      cspec = csp 
   endif
   if (present(speci)) then
      speci = si
   endif
   if (present(specj)) then
      specj = sj
   endif
   if (present(freq)) then
      freq = f
   endif
   if (present(trf)) then
      call sym_fft(nfft,nf,xspec,trf)
   endif

   return

end subroutine mt_transfer

!--------------------------------------------------------------------
! The dual way transfer function
!--------------------------------------------------------------------

subroutine mt_transfer2(iadapt,npts,nfft,dt,xi,xj,tbp,kspec,nf,    &
                       freq,cohe,trf,cspec,speci,specj)

!
!  subroutine to compute transfer function and squared multiple
!  coherence between one input and one output time series using
!  the multitaper method by Thomson.
!  Jackknife estimates for 95% confidence limits will also be 
!  made available in the future. 
!
!  In MT_TRANSFER2 the signal is normalized (deconvolved) by the 
!  average spectrum of the two signals, instead of choosing only
!  one.  
!
!  INPUT
!
!	iadapt		integer 0 - adaptive, 1 - constant weights
!	npts		integer number of points in time series
!       nfft		integer number of point to pad
! 	dt		real, sampling rate of time series
!	xi(npts)	real, data for first series
!       xj(npts) 	real, data for second series
!	tbp		the time-bandwidth product
!	kspec		integer, number of tapers to use
!	nf		integer, number of freq points in spectrum
!
!  OUTPUT
!
!	freq(nf)	real vector with frequency bins
!       cohe(nf)	real, coherence of the two series (0 - 1)
!       trf(npts)       complex array with the transfer function
!	cspec(nf)	complex cross spectrum estimate
!	speci(nf)
!	specj(nf)
!

!********************************************************************

   use spectra

   implicit none

!  Inputs

   integer, intent(in)                      :: iadapt, npts, kspec
   integer, intent(in)                      :: nf, nfft

   real(4), intent(in)                      :: dt, tbp

   real(4), dimension(npts), intent(in)     :: xi, xj

!  Outputs

   real(4),    intent(out), dimension(nf)   :: freq
   real(4),    intent(out), dimension(nf)   :: cohe,speci,specj
   complex(4), intent(out), dimension(nf)   :: cspec 
   complex(4), intent(out), dimension(nf)   :: trf
 
! Working variables

   real(4), dimension(nf,kspec)      :: wt_i, wt_j 
   complex(4), dimension(nfft,kspec) :: yk_i, yk_j
   complex(4), dimension(nf)         :: xspec

   real(4), dimension(nf)            :: wt_scale
   complex(4), dimension(nf,kspec)   :: dyk_i, dyk_j 

   real(4), dimension(kspec)         :: wt_dofs
   real(4)                           :: se

   integer                           :: i, j, iad

!********************************************************************

!   write(6,'(a)') 'Calling multitaper transfer function code'
!   write(6,'(a,3i8)') 'Data and frequency points', npts, nfft, nf

   if (iadapt == 1) then
      iad = 1
   else
      iad = 0
   endif

!
!  Get the spectrum estimate
!

   !if (all(xi==xj)) then    
   !   write(6,'(a)') 'Auto-transfer not applicable'
   !   stop
   !else
      call mtspec( npts,nfft,dt,xi,tbp,kspec,nf,freq,          &
                    speci,yk=yk_i,wt=wt_i,adapt=iad)

      call mtspec( npts,nfft,dt,xj,tbp,kspec,nf,freq,          &
    		    specj,yk=yk_j,wt=wt_j,adapt=iad)
   !endif


!
!  Create the spectra (cannot use spec output, normalized different)
!

   wt_i = min(wt_i,wt_j)
   wt_j = min(wt_i,wt_j)

   wt_scale = sum(wt_i**2, dim=2)  ! Scale weights to keep power 
   do i = 1,kspec
      wt_i(:,i) = wt_i(:,i)/sqrt(wt_scale)
      wt_j(:,i) = wt_j(:,i)/sqrt(wt_scale)
   enddo

   do i = 1,nf
      do j = 1,kspec
         dyk_i(i,j) = wt_i(i,j) * yk_i(i,j)
         dyk_j(i,j) = wt_j(i,j) * yk_j(i,j)
      enddo
   enddo

!  Force a zero mean process


   do i = 1,kspec
      dyk_i(:,i) = dyk_i(:,i) - sum(real(dyk_i(:,i)))/real(nfft)
      dyk_j(:,i) = dyk_j(:,i) - sum(real(dyk_j(:,i)))/real(nfft)
   enddo

   speci = sum(abs(dyk_i)**2, dim=2) 
   specj = sum(abs(dyk_j)**2, dim=2) 

   do i = 1,nf

   !  Degrees of freedom (null hypothesis test)

      wt_dofs = wt_i(i,:)/maxval(wt_i(i,:))

      se = 2.0 * sum(wt_dofs**2)

      xspec(i)  = sum ( dyk_i(i,:) * conjg(dyk_j(i,:)) )

   ! coherence 

      cohe(i)  = (abs(xspec(i)))**2 / (speci(i)*specj(i))
      cspec(i) = xspec(i)

   ! Transfer function

!      trf(i)  = 2. * xspec(i)  / (speci(i)+specj(i))
      trf(i)  = xspec(i)  / (specj(i))

   enddo

   return

end subroutine mt_transfer2



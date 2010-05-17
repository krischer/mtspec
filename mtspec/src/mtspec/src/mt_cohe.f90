subroutine mt_cohe ( npts,dt,xi,xj,tbp,kspec,nf,p,                   &
                     freq,cohe,phase,speci,specj,conf,               &
                     cohe_ci, phase_ci,iadapt )  
            
!
!  Construct the coherence spectrum from the yk's and the 
!  weights of the usual multitaper spectrum estimation. 
!  Note this code uses the real(4) multitaper code. 
!
!  INPUT
!
!	npts		integer number of points in time series
! 	dt		real, sampling rate of time series
!	xi(npts)	real, data for first series
!       xj(npts) 	real, data for second series
!	tbp		the time-bandwidth product
!	kspec		integer, number of tapers to use
!	nf		integer, number of freq points in spectrum
!	p		confidence for null hypothesis test
!
!
!  OPTIONAL INPUT
!
!	iadapt		integer 0 - adaptive, 1 - constant weights
!			default adapt = 1
!
!  OPTIONAL OUTPUTS
!
!	freq(nf)	real vector with frequency bins
!       cohe(nf)	real, coherence of the two series (0 - 1)
!       phase(nf)	the phase at each frequency
!	speci(nf)	real vector with spectrum of first series
!	specj(nf)	real vector with spectrum of second
!	conf(nf)	p confidence value for each freq.
!	cohe_ci(nf,2)	95% bounds on coherence (not larger than 1)
!	phase_ci(nf,2)  95% bounds on phase estimates
!
!	If confidence intervals are requested, then both phase and
!	cohe variables need to be requested as well. 
!
!  Modified
!
!	German Prieto
!	September 2005
!
!	*******************************************************************
!
!	German Prieto
!	October 2007
!  
!	Re-wrote the subroutine to allow optional output argu-
!	ments. 
!
!	******************************************************************* 
!
!	German Prieto
!	February 2008
!  
!	Added optional adaptive or constant weighting, similar to 
!       mt_transfer.
!
!	******************************************************************* 
!

!  calls
!	mtspec
!	


!********************************************************************

   use spectra

   implicit none

!  Inputs

   integer, intent(in) :: npts, kspec, nf

   real(4), intent(in) :: dt, tbp
   real(4), intent(in out) :: p

   real(4), dimension(npts), intent(in) :: xi, xj

!  Optional Input

   integer, optional                        :: iadapt

!  Optional outputs

   real(4), dimension(nf), optional :: freq, speci, specj

   real(4), dimension(nf), optional :: cohe, phase, conf
 
   real(4), dimension(nf,2), optional :: cohe_ci, phase_ci

!  spectra and frequency

   real(4), dimension(nf) :: f, si, sj

   real(4), dimension(nf,kspec)      :: wt_i, wt_j 

   complex(4), dimension(npts,kspec) :: yk_i, yk_j

   real(4), dimension(2)             :: xmean, xvar

   real(4), dimension(nf)            :: wt_scale


!  Coherence freq matrices

   complex(4), dimension(nf,kspec)     :: dyk_i, dyk_j 
   complex(4), dimension(nf)           :: spec

!  Confidence terms

   real(4), dimension(kspec)         :: wt_dofs
   real(4)                           :: se

!  Jackknife

   complex(4), dimension(nf,kspec) :: cross
   real(4), dimension(nf,kspec)    :: spec1, spec2

   complex(4), dimension(nf)       :: cross_all
   real(4), dimension(nf)          :: spec1_all, spec2_all
   real(4), dimension(nf)          :: cohe_all, phase_all

   real(4), dimension(nf,kspec)    :: cohejk, Q 
   real(4), dimension(nf,kspec)    :: qvarjk
   real(4), dimension(nf)          :: qmean, qvar, phvar
   real(4), dimension(nf)          :: qtop, qbot, qval

   complex(4), dimension(nf,kspec) :: phasejk
   complex(4), dimension(nf)       :: phmean

   real(4) :: atanh2
   
!  Others

   integer :: i, j, iad

   real(4), parameter :: pi = 3.14159265358979

!********************************************************************

!   write(6,'(a)') 'Calling multitaper coherence code'
!   write(6,'(a,2i6)') 'Data points and frequency points', npts, nf

   if (present(cohe_ci) .and. .not. present(cohe)) then
      write(6,'(a)') 'If cohe_ci is requested, cohe is needed, stopped'
      stop
   elseif (present(phase_ci) .and. .not. present(phase)) then
      write(6,'(a)') 'If phase_ci is requested, phase is needed, stopped'
      stop
   endif

   if (p>=1. .or. p<=0.) then
      p = 0.95
   endif
   write(6,'(a,f10.7)') 'Confidence for null hypothesis testing ', p


   xmean(1) = sum(xi)/real(npts)
   xvar(1) = (sum((xi - xmean(1))**2))/real(npts-1)
   xmean(2) = sum(xj)/real(npts)
   xvar(2) = (sum((xj - xmean(2))**2))/real(npts-1)

! Adaptive or constant weighting?

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

   if (all(xi==xj)) then    
      write(6,'(a)') 'Auto-coherence not applicable'
      stop
   else
      call mtspec( npts,dt,xi,tbp,kspec,nf,f,          &
                    si,yk=yk_i,wt=wt_i,adapt=iad)

      call mtspec( npts,dt,xj,tbp,kspec,nf,f,          &
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

   si = sum(abs(dyk_i)**2, dim=2) 
   sj = sum(abs(dyk_j)**2, dim=2) 

   if (present(speci)) then
      speci = si
   endif
   if (present(specj)) then
      specj = sj
   endif
   if (present(freq)) then
      freq = f
   endif

   do i=1,nf

   ! coherence and phase
   
      spec(i) = sum ( dyk_i(i,:) * conjg(dyk_j(i,:)) )  

      if (present(cohe)) then
         cohe(i) = (abs(spec(i)))**2 / (si(i)*sj(i))
      endif

      if (present(phase)) then
         phase(i) = atan2( aimag(spec(i)), real(spec(i)) ) 
      endif

   ! Null hypothesis and degrees of freedom

!      wt_dofs = wt_i(i,:)/sqrt(sum(wt_i(i,:)**2)/real(kspec))
      wt_dofs = wt_i(i,:)/maxval(wt_i(i,:))


      se = 2.0 * sum(wt_dofs**2)
 
      if (present(conf)) then
         conf(i)  = 1. - ( (1.0-p)**(1./(se/2. -1.)) )
      endif

   enddo

!  Jackknife estimate for confidence intervals
!  Use the arctanh transformation, close to Gaussian.
!  The term with the m dof is not taken into account in this case
!  It is not needed Eq 2.58 Thomson and Chave

   if (.not. present(cohe_ci) .or. .not. present(phase_ci)) then
      if (present(phase)) then
         phase = phase * (180.0/3.1415926535)
      endif
      return
   endif

   if (.not. present(cohe) .or. .not. present(phase)) then
      cohe_ci  = 0.
      phase_ci = 0.
      if (present(phase)) then
         phase = phase * (180.0/3.1415926535)
      endif
      return
   endif

   !  Compute cross and eigenspectra

   do i = 1,nf
      do j = 1,kspec

         cross(i,j) = (wt_i(i,j)*wt_j(i,j)*yk_i(i,j)*conjg(yk_j(i,j)) )
	
         spec1(i,j) = wt_i(i,j) * abs(yk_i(i,j))**2
         spec2(i,j) = wt_j(i,j) * abs(yk_j(i,j))**2

      enddo
   enddo

   cross_all = sum(cross,dim=2)
   spec1_all = sum(spec1,dim=2)
   spec2_all = sum(spec2,dim=2)

   cohe_all  = abs(cross_all)**2/(spec1_all*spec2_all)
   phase_all = atan2(aimag(cross_all),real(cross_all))

   ! Compute the delete on estimates

   do i = 1,nf
      do j = 1,kspec

         cross(i,j) = cross_all(i) - cross(i,j)
         spec1(i,j) = spec1_all(i) - spec1(i,j)
         spec2(i,j) = spec2_all(i) - spec2(i,j)
 
         cohejk(i,j)  = abs(cross(i,j))**2/(spec1(i,j)*spec2(i,j))
         phasejk(i,j) = cross(i,j)/abs(cross(i,j))   ! Eq 2.61

         Q(i,j)       = atanh2(sqrt(cohejk(i,j))) 

      enddo
      
      qval(i)   = atanh2(sqrt(cohe(i))) 
      qmean(i)  = sum(Q(i,:))/real(kspec)
      phmean(i) = sum(phasejk(i,:))/real(kspec)       ! Eq 2.62

   enddo

   !  Statistics on the Q and phase variables

   do i = 1,nf
      do j = 1,kspec
      
         qvarjk(i,j)   = (Q(i,j) - qmean(i))**2

      enddo

      qvar(i)  = sum(qvarjk(i,:)) * real(kspec-1)/real(kspec)
      phvar(i) = 2.*(real(kspec-1)) * (1.-abs(phmean(i)))   ! Eq 2.63

   enddo

   do i = 1,nf
   
      qtop(i) = (qval(i) + 1.96*sqrt(qvar(i)))
      qbot(i) = (qval(i) - 1.96*sqrt(qvar(i)))

      if (qbot(i)<0.) then
         qbot(i)=0.
      endif
   
      cohe_ci(i,1) = (tanh(qbot(i)))**2
      cohe_ci(i,2) = (tanh(qtop(i)))**2

      phase_ci(i,1) = phase(i) - 1.96*sqrt(phvar(i))
      phase_ci(i,2) = phase(i) + 1.96*sqrt(phvar(i))
 
   enddo

   phase = phase * (180.0/3.1415926535)
   phase_ci = phase_ci * (180.0/3.1415926535)

end subroutine mt_cohe


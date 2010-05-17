subroutine df_spec ( npts,dt,xi,xj,tbp,kspec,nf,                   &
                     freq,df_spectra,df_cohe,df_phase )  
            
!
!  Construct the dual-frequency spectrum from the yk's and the 
!  weights of the usual multitaper spectrum estimation. 
!  The dual-frequency spectrum is saved in a file, needed for 
!  long time series where computer memory breaks down.
!
!  INPUT
!	npts		number of points of both time series
!	dt		sampling rate for both series
!	xi, xj		Vectors with npts points, real(4)
!	tbp		Time-bandwidth product
!	kspec		Number of tapers to use
!	nf		number of points of frequency domain
!
!  OUTPUT
!	freq		vector with freq bins (nf long)
!	df_cohe		MSC, dual-freq coherence (nf,nf) matrix
!	df_phase	dual-freq phase
!	df_spectra	The full complex dual-freq cross
!			coherence. From this we can also obtain
!			df_cohe, df_phase. 
!
!  Modified
!	German Prieto
!	September 2005
!
!	German A. Prieto
!	September 2007
!	Slight rewrite to adjust to newer mtspec codes.
!  
!
!  calls
!	mtspec
!

!********************************************************************

   use spectra

   implicit none

!  Inputs

   integer, intent(in)                  :: npts, kspec, nf
   real(4), intent(in)                  :: dt, tbp
   real(4), dimension(npts), intent(in) :: xi, xj

!  Outputs

   real(4), intent(out), dimension(nf)       :: freq
   real(4), intent(out), dimension(nf,nf)    :: df_cohe, df_phase
   complex(4), intent(out), dimension(nf,nf) :: df_spectra

!  spectra and frequency

   real(4), dimension(nf)            :: speci, specj
   real(4), dimension(nf,kspec)      :: wt_i, wt_j 
   complex(4), dimension(npts,kspec) :: yk_i, yk_j
   real(4), dimension(nf)            :: wt_scale

!  Dual freq matrices

   complex(4), dimension(nf,kspec)   :: dyk_i, dyk_j 
   
!  Others

   integer :: i, j

   real(4), parameter :: pi = 3.14159265358979

!********************************************************************

   write(6,'(a,2i5)') 'Data points and frequency points', npts, nf

!
!  Get the spectrum estimate
!  if xi is xj, then just do one estimate of the spectrum
!

   if (all(xi==xj)) then    
      call mtspec( npts,dt,xi,tbp,kspec,nf,freq,          &
   	            speci,yk=yk_i,wt=wt_i)
      specj = speci
      yk_j = yk_i
      wt_j = wt_i
   else
      call mtspec( npts,dt,xi,tbp,kspec,nf,freq,          &
                    speci,yk=yk_i,wt=wt_i)

      call mtspec( npts,dt,xj,tbp,kspec,nf,freq,          &
    		    specj,yk=yk_j,wt=wt_j)
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

   speci = sum(abs(dyk_i)**2, dim=2) 
   specj = sum(abs(dyk_j)**2, dim=2) 

   do i=1,nf

      if (mod(i,1000) == 0) then
         write(6,'(a,i5,a,i5)') 'Loop ',i , ' of ', nf 
      endif

   ! df_spectrum and coherence
   
      do j = 1,nf
         
         df_spectra(i,j) = sum ( dyk_i(i,:) * conjg(dyk_j(j,:)) )   

         df_cohe(i,j) = (abs(df_spectra(i,j)))**2 / (speci(i)*specj(j))

         df_phase(i,j) = atan2( aimag(df_spectra(i,j)),real(df_spectra(i,j)) ) 

      enddo
      
   enddo

end subroutine df_spec









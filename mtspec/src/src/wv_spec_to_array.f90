subroutine wv_spec_to_array ( npts,dt,x, tbp,kspec,                   &
                     filter, fbox)
           

!
!  Construct the Wigner-Ville spectrum from the dual-frequency 
!  spectrum, by taking the inverse FFT, to the 45 degrees
!  rotated df_spectrum.
!  Both df_spec and the Wigner-Ville spectrum are save in a file.
!  
!  Filter	0	No filter
!		1	Boxcar filter
!		2	Guassian Filter
!
!  fbox		Width of the filter to use (for boxcar and Gaussian)
!

!********************************************************************

   use spectra

   implicit none

!  Inputs

   integer, intent(in) :: npts, kspec, filter

   real(4), intent(in) :: dt, tbp, fbox

   real(4), dimension(npts), intent(in) :: x

!  spectra and frequency

   integer :: nf, nf2, nfft 

   real(4), dimension(:), allocatable :: speci, specj

   real(4), dimension(:,:), allocatable :: wt_i, wt_j 

!   complex(4), dimension(2*(npts+mod(npts,2))-1,kspec) :: yk_i, yk_j
   complex(4), dimension(:,:), allocatable :: yk_i, yk_j

   real(4), dimension(:), allocatable              :: wt_scale

!  spectra and frequency

   real(4), dimension(:), allocatable              :: freq

!  Dual freq matrices

   complex(4), dimension(:,:), allocatable     :: dyk_i, dyk_j 
   
   real(4), dimension(:), allocatable              :: df_cohe
 
   complex(4), dimension(:), allocatable           :: df_spec

!  Others

   integer :: i, j, k

   character (len = 100) :: fmt, fmt2, file1, file2

!  Matrix rotation

   integer :: indices, ncol, gmax

   complex(4), dimension(:), allocatable :: x2

!  Filtering matrix

   real(4) :: sigma
   real(4), dimension(:), allocatable :: x_filt, i_filt, df_filt


!********************************************************************

   nf2 = npts

   nfft = 2*npts
   nf   = nfft/2 + 1

   allocate(speci(nf))
   allocate(specj(nf))
   allocate(wt_i(nf,kspec))
   allocate(wt_j(nf,kspec))
   allocate(wt_scale(nf))
   allocate(yk_i(nfft,kspec))
   allocate(yk_j(nfft,kspec))

   allocate(freq(nf))
   allocate(dyk_i(nf,kspec))
   allocate(dyk_j(nf,kspec))

   allocate(df_cohe(nf))
   allocate(df_spec(nf))

   allocate(x2(npts))
   allocate(x_filt(nf))
   allocate(i_filt(nf))
   allocate(df_filt(nf))

   write(6,'(a,2i5)') 'Data points and frequency points', npts, nf2

!
!  Get the spectrum estimate
!

 call mtspec ( npts,nfft,dt,x,tbp,kspec,nf,freq,          &
                  speci,yk=yk_i,wt=wt_i)
 specj = speci
 yk_j = yk_i
 wt_j = wt_i

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

!  Filter vector

   i_filt = real( (/((i-1), i=1,nf)/) )

   sigma  = real(nf)/fbox

   if (filter==2) then
   
      write(6,'(a)') 'Applying Gaussian filter'
      
   ! Guassian Curve

      x_filt = exp( real(-1./2.) *     &
               (sigma * (i_filt-real(nf/2))/real(nf))**2);
      df_filt = exp( real(-1./2.) *    &
                (sigma * (i_filt/2.)/real(nf))**2);

   elseif (filter==1) then
 
      write(6,'(a)') 'Applying Boxcar filter'

   ! Boxcar filter
  
      do i =1,nf
      
         if (abs(i_filt(i)-real(nf/2)) > fbox) then
            x_filt(i) = 0.
         else
            x_filt(i) = 1.
         endif

         if (i_filt(i)/2. > fbox) then
            df_filt(i) = 0.
         else
            df_filt(i) = 1.
         endif

      enddo

   else 
   
   ! Flat

      x_filt  = 1.
      df_filt = 1.

   endif

   file1 = 'wv.dat'
   file2 = 'df.dat'

   open(12,file=file1,form='formatted')
 
   open(13,file=file2,form='formatted')

   write(fmt,'(i12)') npts
   fmt = '(' // trim(adjustl(fmt)) // 'E16.7)'

   write(fmt2,'(i12)') nf
   fmt2 = '(' // trim(adjustl(fmt2)) // 'E16.7)'

   do i=1,nf

      if (mod(i,1000) == 0) then
         write(6,'(a,i5,a,i5)') 'Loop ',i , ' of ', nf 
      endif

   ! df_spectrum and coherence
   
      do j = 1,nf
        
         df_spec(j) = sum ( dyk_i(i,:) * conjg(dyk_j(j,:)) )   

         df_cohe(j) = (abs(df_spec(j)))**2 / (speci(i)*specj(j))
 
	 ! Filter

         k = nint( sign(0.5,real(j-i))-sign(0.5,-real(j-i)) ) * (j-i) +1 

         df_cohe(j) = df_cohe(j) * df_filt(k)

      enddo
      
      write(13,fmt2) ( real(df_cohe(:)) )

   ! Wigner-Vlle

      gmax = min(i-1,nf-i,nint(nf/2.)-1)

      ncol = i
      
      x2 = ( 0., 0. )

      do j = -gmax,gmax,1
         
         indices = mod(npts+j,npts) + 1

         x2(indices) = sum ( dyk_i(ncol+j,:) * conjg(dyk_j(ncol-j,:)) ) 

         x2(indices) = x2(indices) * x_filt((nf/2)+1 + j)

      enddo

      call ifft4(x2,npts)

      write(12,fmt) ( real(x2(:)) )

 
   enddo

   close(12)
   close(13)

   deallocate(speci, specj, wt_i, wt_j, wt_scale, yk_i, yk_j)
   deallocate(freq,dyk_i,dyk_j,df_cohe, df_spec)
   deallocate(x2,x_filt,i_filt, df_filt)   

end subroutine wv_spec_to_array

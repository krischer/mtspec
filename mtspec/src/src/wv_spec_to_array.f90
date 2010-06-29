subroutine wv_spec_to_array ( npts,dt,x,x3, frac,tfrac,nfmin,nfmax, tbp,kspec,&
                     filter, fbox, verb)

!  Construct the Wigner-Ville spectrum from the dual-frequency 
!  spectrum, by taking the inverse FFT, to the 45 degrees
!  rotated df_spectrum.
!  Only the Wigner-Ville spectrum are save in a file.
!  
!  Filter	0	No filter
!		1	Boxcar filter
!		2	Guassian Filter
!
!  fbox		Width of the filter to use (for boxcar and Gaussian)
! 
!  Modified the original routine to allow for a variable number of
!  frequency points. Does no more seperately calculate the dual
!  frequency spectrum. frac specifies the number of frequency 
!  points to use as a fraction of npts.

!********************************************************************

   use spectra

   implicit none

!  Inputs

   integer, intent(in) :: npts, kspec, filter, frac, tfrac, nfmin, nfmax

   real(4), intent(in) :: dt, tbp, fbox

   real(4), dimension(npts), intent(in) :: x

!  spectra and frequency

   integer :: nf, nf2, nfft

   real(4), dimension(:), allocatable :: spec

   real(4), dimension(:,:), allocatable :: wt

   complex(4), dimension(:,:), allocatable :: yk

   real(4), dimension(:), allocatable              :: wt_scale

!  spectra and frequency

   real(4), dimension(:), allocatable              :: freq

!  Dual freq matrices

   complex(4), dimension(:,:), allocatable     :: dyk
   
!  Others

   integer :: i, j, k, l, m, n, tind

!  Verbose

   character (len=1),                optional  :: verb
   integer                                     :: v

!  Matrix rotation

   integer :: indices, ncol, gmax

   complex(4), dimension(:), allocatable :: x2

!  Filtering matrix

   real(4) :: sigma, tmax
   real(4), dimension(:), allocatable :: x_filt, i_filt, df_filt

! Output

   complex(4), dimension(int((nfmax - nfmin)/(2*frac)), int(npts/tfrac)) :: x3

!********************************************************************

   ! Determine whether or not to use verbose mode.
   if (present(verb)) then 
      if (index(verb,'n') == 0) then
         v = 1 
      endif
   else
      v = 0
   endif

   nf2 = npts

   nfft = 2*npts
   nf   = nfft/2 + 1

   allocate(spec(nf))
   allocate(wt(nf,kspec))
   allocate(wt_scale(nf))
   allocate(yk(nfft,kspec))

   allocate(freq(nf))
   allocate(dyk(nf,kspec))

   allocate(x2(npts))
   allocate(x_filt(nf))
   allocate(i_filt(nf))
   allocate(df_filt(nf))

   if (v == 1) then
   write(6,'(a,3i5)') 'Data points and frequency points and steps', npts, nf2, frac
   endif

!
!  Get the spectrum estimate
!

 call mtspec ( npts,nfft,dt,x,tbp,kspec,nf,freq,          &
                  spec,yk=yk,wt=wt)
 deallocate(spec)

!
!  Create the spectra (cannot use spec output, normalized different)
!

   wt_scale = sum(wt**2, dim=2)  ! Scale weights to keep power 
   do i = 1,kspec
      wt(:,i) = wt(:,i)/sqrt(wt_scale)
   enddo

   deallocate(wt_scale)

   do i = 1,nf
      do j = 1,kspec
         dyk(i,j) = wt(i,j) * yk(i,j)
      enddo
   enddo

!  Filter vector

   i_filt = real( (/((i-1), i=1,nf)/) )

   sigma  = real(nf)/fbox

   if (filter==2) then
   
      if (v == 1) then
         write(6,'(a)') 'Applying Gaussian filter'
      endif
      
   ! Guassian Curve

      x_filt = exp( real(-1./2.) *     &
               (sigma * (i_filt-real(nf/2))/real(nf))**2);
      df_filt = exp( real(-1./2.) *    &
                (sigma * (i_filt/2.)/real(nf))**2);

   elseif (filter==1) then
 
      if (v == 1) then
         write(6,'(a)') 'Applying Boxcar filter'
      endif

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

   ! Discretize/downsample in frequency space
   k = 1
   do i= nfmin, nfmax, 2 * frac

      if (v == 1) then
          if (mod(i,101) == 0) then
             write(6,'(a,i5,a,i5)') 'Loop ',i , ' of ', nf 
          endif
      endif

   ! Wigner-Ville

      gmax = min(i-1,nf-i,nint(nf/2.)-1)

      ncol = i
      
      x2 = ( 0., 0. )

      do j = -gmax,gmax,1
         
         indices = mod(npts+j,npts) + 1

         x2(indices) = sum ( dyk(ncol+j,:) * conjg(dyk(ncol-j,:)) ) 

         x2(indices) = x2(indices) * x_filt((nf/2)+1 + j)

      enddo

      call ifft4(x2,npts)

      ! Discretize/downsample in time space
      ! Copy to output array
      n = 1
      do l=1, npts, tfrac
          tmax = 0.0
          do m=0, tfrac - 1
              if ((real(x2(l+m))**2 + aimag(x2(l+m))**2) > tmax) then
                  tind = l+m
                  tmax = x2(tind)
              endif
          enddo
          x3(k,n) = x2(tind)
          n = n + 1
      enddo
      k = k + 1

 
   enddo

   deallocate(wt, yk)
   deallocate(freq,dyk)
   deallocate(x2,x_filt,i_filt, df_filt)   

end subroutine wv_spec_to_array

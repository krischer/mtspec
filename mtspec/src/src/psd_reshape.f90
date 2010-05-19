subroutine psd_reshape(npts,nf,kspec,fcritical,pin,vn,yk_in,yk,sline)

!
!  Modified
!	German Prieto
!	April 2006
!
!  Reshape eigenft's around significant spectral lines
!  The "significant" means above fcritical probability (0.95)
!  If probability is large at neighbouring frequencies, I will 
!  only remove the largest probability energy. 
!
!  INPUT
!  	npts	number of time domain points
!	nf	frequency vector length
!
!  calls nothing
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in) :: npts, nf, kspec

   real(4), intent(in) :: fcritical

   real(4), dimension(nf), intent(in) :: pin

   real(8), dimension(npts,kspec), intent(in) :: vn

   complex(8), dimension(npts,kspec), intent(in) :: yk_in

!  Output

   complex(8), dimension(npts,kspec), intent(out) :: yk

   real(8), dimension(nf), intent(out) :: sline

!  Other

   integer :: i, j, nl, jj, k
   integer, dimension(nf-1) :: iloc

   real(4), dimension(nf) :: p

   complex(8), dimension(nf) :: mu
   real(8), dimension(kspec) :: vn0

!  The Vk = fft(vn)

   complex(8), dimension(npts,kspec) :: Vk

   real(8), dimension(2,npts)        :: a
   
   complex(8), dimension(npts)       :: dummy_Vk
   
!********************************************************************

!  Check number of values nl over critical statistic

   p = pin		! do not overwrite pin
   sline = 0.d0

   nl = 0
   do i = 1,nf-1
 
      if (p(i) <= fcritical) then
         cycle
      endif

      do j = i+1,nf
         if (p(j) <= fcritical) then
            nl = nl + 1
            iloc(nl) = i
            exit
         elseif (p(j) > fcritical .and. p(j) > p(i) ) then
            exit
         elseif (p(j) > fcritical .and. p(j) < p(i) ) then
            p(j) = 0.
            if (j == nf) then
               nl = nl + 1
               iloc(nl) = i
            endif
            cycle
         endif 
    
      enddo

   enddo

   if (p(nf) > fcritical .and. p(nf) > p(nf-1) ) then
      nl = nl + 1
      iloc(nl) = nf
   endif

   if (nl == 0) then 
      yk = yk_in
      sline = 0.d0
      return
   endif

   yk = yk_in

!  Calculate the mean amplitude of line components at line frequencies

   vn0 = sum(vn,dim=1)

   do i = 1,nl

      mu(i) = sum(vn0*yk(iloc(i),:)) / sum(vn0**2)

   enddo


!  Compute the Vk's to reshape
!  The Vk's normalized to have int -1/2 1/2 Vk**2 = 1 
!  This is obtained from fft already is sum(vn**2) = 1



   do i = 1,kspec

      a(1,:) = vn(:,i)
      a(2,:) = 0.d0

      dummy_Vk = cmplx(a(1,:),a(2,:),kind=8)
      
      call fft(dummy_Vk,npts)
     
      Vk(:,i) = dummy_Vk

   enddo

!  Remove mean value for each spectral line

   do i = 1,nl

      do j = 1, npts

         jj = j - iloc(i) + 1
         if (jj < 1) then
            jj = jj + npts
         endif
         
         yk(j,:) = yk(j,:) - mu(i)*Vk(jj,:)
  
         do k = 1,kspec
            sline(iloc(i)) = sline(iloc(i)) +   &
                             1.d0/dble(kspec)*abs(mu(i)*Vk(jj,k))**2
         enddo

      enddo 
   
   enddo

     
end subroutine psd_reshape

!--------------------------------------------------------------------
! The padding version
!--------------------------------------------------------------------

subroutine psd_reshape_pad(npts,nfft,nf,kspec,fcritical,pin,vn,yk_in,yk,sline)

!
!  Modified
!	German Prieto
!	February 2007
!
!  Reshape eigenft's around significant spectral lines
!  The "significant" means above fcritical probability (0.95)
!  If probability is large at neighbouring frequencies, I will 
!  only remove the largest probability energy. 
!
!  INPUT
!  	npts	number of time domain points
!       nfft    number of points in complex spectrum
!	nf	frequency vector length
!
!  calls nothing
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in) :: npts, nfft, nf, kspec

   real(4), intent(in) :: fcritical

   real(4), dimension(nf), intent(in) :: pin

   real(8), dimension(npts,kspec), intent(in) :: vn

   complex(8), dimension(nfft,kspec), intent(in) :: yk_in

!  Output

   complex(8), dimension(nfft,kspec), intent(out) :: yk

   real(8), dimension(nf), intent(out) :: sline

!  Other

   integer :: i, j, nl, jj, k
   integer, dimension(nf-1) :: iloc

   real(4), dimension(nf) :: p

   complex(8), dimension(nf) :: mu
   real(8), dimension(kspec) :: vn0

!  The Vk = fft(vn)

   complex(8), dimension(nfft,kspec) :: Vk

   real(8), dimension(2,nfft)        :: a
   
   complex(8), dimension(nfft)       :: dummy_Vk
   
!********************************************************************

!  Check number of values nl over critical statistic

   p = pin		! do not overwrite pin
   sline = 0.d0

   nl = 0
   do i = 1,nf-1
 
      if (p(i) <= fcritical) then
         cycle
      endif

      do j = i+1,nf
         if (p(j) <= fcritical) then
            nl = nl + 1
            iloc(nl) = i
            exit
         elseif (p(j) > fcritical .and. p(j) > p(i) ) then
            exit
         elseif (p(j) > fcritical .and. p(j) < p(i) ) then
            p(j) = 0.
            if (j == nf) then
               nl = nl + 1
               iloc(nl) = i
            endif
            cycle
         endif 
    
      enddo

   enddo

   if (p(nf) > fcritical .and. p(nf) > p(nf-1) ) then
      nl = nl + 1
      iloc(nl) = nf
   endif

   if (nl == 0) then 
      yk = yk_in
      sline = 0.d0
      return
   endif

   yk = yk_in

!  Calculate the mean amplitude of line components at line frequencies

   vn0 = sum(vn,dim=1)

   do i = 1,nl

      mu(i) = sum(vn0*yk(iloc(i),:)) / sum(vn0**2)

   enddo


!  Compute the Vk's to reshape
!  The Vk's normalized to have int -1/2 1/2 Vk**2 = 1 
!  This is obtained from fft already is sum(vn**2) = 1

   do i = 1,kspec
  
      a = 0.d0 
      a(1,1:npts) = vn(:,i)
      a(2,:) = 0.d0

      dummy_Vk = cmplx(a(1,:),a(2,:),kind=8)
      
      call fft(dummy_Vk,nfft)
     
      Vk(:,i) = dummy_Vk

   enddo


!  Remove mean value for each spectral line

   do i = 1,nl

      do j = 1, nfft

         jj = j - iloc(i) + 1
         if (jj < 1) then
            jj = jj + nfft
         endif
        
         yk(j,:) = yk(j,:) - mu(i)*Vk(jj,:)
        
         do k = 1,kspec
            sline(iloc(i)) = sline(iloc(i)) +   &
                             1.d0/dble(kspec)*abs(mu(i)*Vk(jj,k))**2
         enddo
         
      enddo 

   enddo

     
end subroutine psd_reshape_pad

   

  
   

  

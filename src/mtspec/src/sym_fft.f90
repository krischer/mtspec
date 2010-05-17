!--------------------------------------------------------------------
! Mirror complex number to make ifft real
!--------------------------------------------------------------------

subroutine sym_fft(nfft,nf,spec,s_spec)

!
!  Code to take a complex number and make it symmetric in such
!  a way to obtain a real number if IFFT is used. It knows how 
!  to handle odd and even number of points.
!
!  Mirror complex number to make ifft real
!
!  First created:
!  	German Prieto
!	February 25 2007
!

!********************************************************************

   implicit none

   integer, intent(in)                      :: nfft, nf

   complex(4), dimension(nf), intent(in)    :: spec

   complex(4), dimension(nfft), intent(out) :: s_spec

!  Processing

   integer :: i 

!********************************************************************

   if (nfft <= 0 .or. nf <= 0) then
      write(6,'(a)') 'Error, size of number of points is not valid'
      stop
   endif

   if (mod(nfft,2)==0) then

      s_spec(1)  = spec(1)
      s_spec(nf) = spec(nf)

      do i = 2, nf-1

         s_spec(i)          = spec(i)
         s_spec(nfft-(i-2)) = conjg(spec(i))

      enddo

      
   else
 
      s_spec(1)  = spec(1)

      do i = 2, nf

         s_spec(i)          = spec(i)
         s_spec(nfft-(i-2)) = conjg(spec(i))

      enddo

   endif

end subroutine sym_fft




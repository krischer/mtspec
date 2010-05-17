subroutine oct_spec(nf,freq,spec,nfoct,foct,soct,ifreq)

!
! Subroutine to "resample" the spectrum to an octave scale. 
! Both foct and soct need to have the adecuate size for this 
! to work. The size nfoct is 
!
!    nint( (log10(lfmax)-log10(fmin)) * 40. )
!
! The 1/12th octave scale used here allways has 
! 0.1, 1.0, 10., ... as standard values. It has also 40 samples 
! per decade (e.g., between 1.0 and 10.0)
!
! The 1/12th octave scale is defined as
!
!	f1 = 1000 Hz. 	
!	f2 = f1 / [10**(3/[10*12])]
!	f3 = f2 / [10**(3/[10*12])]
!	...
!
! The edge band frequencies are defined as
!
!	fupper = fn * 2**(1/24)
!	flower = fn / 2**(1/24)
!
! Only the first ifreq values are non-zero. 
!

!********************************************************************

   implicit none

   integer,                   intent(in)  :: nf, nfoct
   real(4), dimension(nf),    intent(in)  :: freq, spec
   integer,                   intent(out) :: ifreq
   real(4), dimension(nfoct), intent(out) :: foct, soct

   real(4) :: fmin, fmax
   real(4) :: logfmin, ilogf0, f0, lfmin, lfmax
   real(4) :: f, flow, fupp, fbnd

   integer :: nfreq, fcnt, i1, i2
   integer, dimension(1) :: ifupp, iflow
 
!********************************************************************

   foct = 0.
   soct = 0.

   fmin = minval(freq,mask=freq>0.)
   fmax = maxval(freq)

   lfmin = log10(fmin)
   lfmax = log10(fmax)

   nfreq = nint((lfmax-lfmin)*40.)
 
   if ( nfoct /= nfreq ) then
      write(6,*)'Size of array is wrong, returned'
      write(6,*) nfoct, nfreq
      return
   endif

   ! Find previous decade to start with. 

   logfmin = log10(fmin)

   ilogf0 = floor(logfmin)

   f0 = 10.**(real(ilogf0))

   f  = f0
   ifreq = 0 
   fbnd  = 10.**(3./(10.*24.))
   
   do while (f < fmax)

      f = f *  (10.**(3./(10.*12.)))
      if (f < fmax .and. f > fmin ) then

         fupp = f * fbnd 
         flow = f / fbnd
 
         fcnt = count(freq<=fupp .and. freq>=flow)
         if (fcnt <= 0) then
            cycle
         endif

         ifreq = ifreq+1

         foct(ifreq) = f

         iflow = minloc(freq,freq >= flow)
         ifupp = maxloc(freq,freq <= fupp)
         i1 = iflow(1)
         i2 = ifupp(1)

         soct(ifreq) = sum(spec(i1:i2))/real(fcnt)

      endif

   enddo

end subroutine oct_spec



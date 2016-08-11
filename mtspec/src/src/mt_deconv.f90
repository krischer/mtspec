subroutine mt_deconv( &
    npts, nfft, dt, xi, xj, tbp, kspec, nf, iadapt,     &
    freq, tfun, spec_ratio, speci, specj, demean, fmax)


!
!  Changed some things for the python wrappper - mainly made all arguments
!  mandatory, as it otherwise does not really work with ctypes.
!
!  Generate a deconvolution between two time series from
!  the yk's and the weights of the usual multitaper spectrum
!  estimation.
!  The code more or less follows the paper
!  Receiver Functions from multiple-taper spectral corre-
!  lation estimates
!  J. Park and V. Levin., BSSA 90#6 1507-1520
!
!  It also uses the code based on dual frequency I created in
!  GA Prieto, Vernon, FL , Masters, G, and Thomson, DJ (2005),
!  Multitaper Wigner-Ville Spectrum for Detecting Dispersive
!  Signals from Earthquake Records, Proceedings of the
!  Thirty-Ninth Asilomar Conference on Signals, Systems, and
!  Computers, Pacific Grove, CA., pp 938-941.
!
!  Note this code uses the real(4) multitaper code.
!
!  INPUT
!   npts    integer number of points in both series
!   nfft    number of fft points, usually nfft>npts to pad.
!   dt  real, sampling rate of time series
!   xi, xj  real, data series with npts points
!   tbp the time-bandwidth product
!   kspec   integer, number of tapers to use
!   nf  integer, number of freq point in the spectrum
!   iadapt      integer 0 - adaptive, 1 - constant weights
!   demean          if present, force complex TF to be demeaned.
!   fmax        maximum frequency for lowpass cosine filter
!
!  OUTPUT
!   freq(nf)    real vector with frequency bins
!   tfun(nfft)  real, time domain deconvolve series
!   spec_ratio(nf)  real, spectral ratio of two spectra
!   speci(nf)   real vector with spectrum of first series
!   specj(nf)   real vector with spectrum of second series
!
!
!  Modified
!
!   German Prieto
!   February 25 2007
!
!   *******************************************************************
!
!   German Prieto
!   September 30 2007
!
!   Re-wrote the subroutine to allow optional output argu-
!   ments.
!
!   *******************************************************************
!

!
!  calls
!   mtspec, ifft4, sym_fft
!

!********************************************************************

    use spectra

    implicit none

!  Inputs

    integer, intent(in)                     :: npts, kspec, nf, nfft, iadapt
    real(4), intent(in)                     :: dt, tbp
    real(4), dimension(npts), intent(in)    :: xi, xj

    integer, intent(in)                     :: demean
    real(4), intent(in)                     :: fmax

    real(4), dimension(nf), intent(out)     :: freq
    real(4), dimension(nfft), intent(out)   :: tfun
    real(4), dimension(nf), intent(out)     :: spec_ratio
    real(4), dimension(nf), intent(out)     :: speci, specj

!  spectra and frequency

    real(4), dimension(nf)            :: f, si, sj
    real(4), dimension(nf,kspec)      :: wt_i, wt_j
    complex(4), dimension(nfft,kspec) :: yk_i, yk_j
    real(4), dimension(nf)            :: wt_scale

!  Confidence terms

!  Coherence freq matrices

    complex(4), dimension(nf,kspec)     :: dyk_i, dyk_j
    complex(4), dimension(nf)           :: xspec

!  Time domain conversion

    real(4),    dimension(nf)           :: ffilt
    complex(4), dimension(nfft)         :: s_spec

!  Others
    integer :: i, j, iad
    real(4) :: eps
    real(4), parameter :: pi = 3.14159265358979


!
!  Get the spectrum estimate
!
   call mtspec( npts,nfft,dt,xi,tbp,kspec,nf,f,      &
         si,yk=yk_i,wt=wt_i,adapt=iadapt)
   call mtspec( npts,nfft,dt,xj,tbp,kspec,nf,f,       &
         sj,yk=yk_j,wt=wt_j,adapt=iadapt)

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


    speci = si
    specj = sj
    freq = f

    do i=1,nf
        ! Deconvolution
        xspec(i) = sum ( dyk_i(i,:) * conjg(dyk_j(i,:)) )
        xspec(i) = xspec(i) / (sj(i))
    enddo

    spec_ratio = sqrt(si/sj)

    !  Apply a cos^2 filter before
    if (fmax > 0) then
        if (fmax < f(nf) .and. fmax > f(2) ) then
            print *, 'Filter'
            do i = 1,nf
                if (f(i) < fmax) then
                    ffilt(i) = (cos(pi*f(i)/(2.*fmax)))**2
                else
                    ffilt(i) = 0.0
               endif
            enddo
            xspec = xspec * ffilt * (2.*f(nf)/fmax)
      endif
   endif

    ! Force zero mean
    if (demean > 0) then
        xspec = xspec - sum(xspec)/real(nf)
    endif

    !  Get the time domain deconvolution, correct units
    call sym_fft(nfft,nf,xspec,s_spec)

    call ifft4(s_spec,nfft)

    tfun = real(s_spec)/((real(nfft)))

    return

end subroutine mt_deconv

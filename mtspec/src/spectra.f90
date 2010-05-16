module spectra

!
!  Modifications
!	Aug 24 / 2006
!	Added a separate routine to do the jackknife estimates, in 
!	order to be able to add the Quadratic Inverse codes, before
!	jackknifing. The code will run faster this way. Qi theory
!	needs preliminary weights from adaptspec.f95. 
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
!	September / 2007
!
!	Major rearranging. I separated all subroutines from the 
!	SPECTRA MODULE. Now, the module is located in this file 
!	(spectra.f90) and contains all the interfaces for the main 
!	library subroutines. 
!	This includes interfaces for mtspec.f90, so that the user can 
!	call real(8), real(4), matrix version, etc and also to allow 
!	the use of optional arguments. 
!

!
!  Interface notation
!
!      		d	double precision version
!		r	single precision version
!               m	matrix, single precision
!               pad     for request of padded signals
!               short   sine_psd for short outputs (only spec)
!      		z	double precision complex version
!		c	single precision complex version

!

!********************************************************************
! Begin interface descriptions
!********************************************************************

   interface mtspec
      module procedure mtspec_d, mtspec_r, mtspec_m, mtspec_pad, &
                       mtspec_c, mtspec_yk
   end interface mtspec

   interface sine_psd
      module procedure sine_psd, sine_psd_short
   end interface sine_psd

   interface eigenft
      module procedure eigenft, eigenft_pad, eigenft_c
   end interface eigenft

   interface ftest
      module procedure ftest, ftest_pad
   end interface ftest

   interface psd_reshape
      module procedure psd_reshape, psd_reshape_pad
   end interface psd_reshape

   interface fft
      module procedure fft_d, fft_r, fft_c, fft_z
   end interface fft 

   interface ifft
      module procedure ifft_d, ifft_r, ifft_c, ifft_z
   end interface ifft 

!********************************************************************
! End interface descriptions
!********************************************************************

!
!  The subroutines
!	
!	MTSPEC interface
!	
!	The original subroutine performs computation in double precision, 
!	sometimes this is not needed. So I created this interface to 
!	allow single precision output. Be aware that all computations 
!	are still double precision, only the input/output is going
!	to be single precision.
!
!	Additional subroutines are now available to deal with multiple
!       computations of the PSD (mtspec_m) with a matrix input as the 
!       time series. Also, mtspec_pad allows the padding of the series 
!       with zeros (after tapering) by requesting an NFFT long freq. 
!       domain transform (NFFT > NPTS). It may be useful for correla-
!       tions to avoid cycle-correlations. 
!	
!	Changed subroutine name RESHAPE (old) to psd_reshape (new) 
!	since reshape is an intrinsic F90 function.
!
!

contains

include 'mtspec.f90'
include 'sine_psd.f90'
include 'eigenft.f90'
include 'ftest.f90'
include 'psd_reshape.f90'
include 'fft.f90'
include 'ifft.f90'

end module spectra

module mvspectra

!
!  Module for the multivariate subroutines, to allow for optional
!  arguments and interfaces. 
!
!  Modifications
!
!  March 2008
!
!  Separated the multivariate subroutines from the spectra.mod module
!  due to incompatibilities with the Sun compilers. 
!
!********************************************************************
! Interface descriptions
!********************************************************************
!

   interface

      subroutine mt_cohe( npts,dt,xi,xj,tbp,kspec,nf,p,                   &
               freq,cohe,phase,speci,specj,conf,cohe_ci, phase_ci,iadapt ) 

         integer, intent(in) :: npts, kspec, nf
         real(4), intent(in) :: dt, tbp
         real(4), intent(in out) :: p
         real(4), dimension(npts), intent(in) :: xi, xj
         integer, optional                    :: iadapt
         real(4), dimension(nf),   optional :: freq, speci, specj
         real(4), dimension(nf),   optional :: cohe, phase, conf
         real(4), dimension(nf,2), optional :: cohe_ci, phase_ci

      end subroutine mt_cohe   

      subroutine mt_deconv ( npts,nfft,dt,xi,xj,tbp,kspec,nf,     &
                     freq,tfun,spec_ratio,speci,specj,iadapt,demean,fmax )  

         integer, intent(in) :: npts, kspec, nf, nfft
         real(4), intent(in) :: dt, tbp
         real(4), dimension(npts), intent(in) :: xi, xj
         integer, optional                   :: iadapt, demean
         real(4), optional		     :: fmax
         real(4), dimension(nf), optional    :: freq
         real(4), dimension(nfft), optional  :: tfun
         real(4), dimension(nf), optional    :: spec_ratio
         real(4), dimension(nf), optional    :: speci, specj

      end subroutine mt_deconv 

      subroutine mt_transfer(npts,nfft,dt,xi,xj,tbp,kspec,nf,    &
                       freq,cohe,trf,cspec,speci,specj,iadapt,demean)

         integer, intent(in)                     :: npts, kspec
         integer, intent(in)                     :: nf, nfft
         real(4), intent(in)                     :: dt, tbp
         real(4), dimension(npts), intent(in)    :: xi, xj
         integer,                     optional   :: iadapt, demean
         real(4),    dimension(nf),   optional   :: freq
         real(4),    dimension(nf),   optional   :: cohe
         complex(4), dimension(nf),   optional   :: cspec
         complex(4), dimension(nfft), optional   :: trf
         real(4), dimension(nf), optional        :: speci, specj


      end subroutine mt_transfer

      subroutine sine_cohe (npts,dt,x1,x2,ntap,ntimes,fact,nf,p,     &
               freq,cohe,phase,speci,specj,kopt,conf,cohe_ci,phase_ci)

         integer, intent(in)                  :: npts, nf 
         integer, intent (in out)             :: ntap, ntimes
         real(4), intent(in)                  :: dt 
         real(4), intent(in out)              :: fact, p
         real(4), intent(in), dimension(npts) :: x1, x2
         integer, dimension(nf),   optional   :: kopt
         real(4), dimension(nf),   optional   :: speci, specj, freq
         real(4), dimension(nf),   optional   :: cohe, phase, conf
         real(4), dimension(nf,2), optional   :: cohe_ci, phase_ci

      end subroutine sine_cohe

   end interface

end module mvspectra

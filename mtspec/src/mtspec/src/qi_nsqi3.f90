subroutine qi_nsqi3(npts,tbp,kspec,nf,lambda,vn,yk,wt,  &
                 qispec,qislope,qiquad,nsdiff,nsd2,    &
                 cte_var,slope_var,qiquad_var,         &
                 nscte_var,nsslope_var,nsquad_var,A)

!
!  Calculate the Stationary Inverse Theory Spectrum.
!  Basically, compute the spectrum inside the innerband. 
!
!  This approach is very similar to 
!  D.J. Thomson (1990).
!
!  These subroutine follows the paper:
!  G. A. Prieto, R. L. Parker, D. J. Thomson, F. L. Vernon, 
!  and R. L. Graham (2007), Reducing the bias of multitaper 
!  spectrum estimates,  Geophys. J. Int., 171, 1269-1281. 
!  doi: 10.1111/j.1365-246X.2007.03592.x.
!
!
!  NOTE:
!
!  In here I have made the Chebyshev polinomials unitless, 
!  meaning that the associated parameters ALL have units 
!  of the PSD and need to be normalized by 1/W for \alpha_1, 
!  1/W**2 for \alpha_2, etc., 
!

!
!  INPUT
!
!	npts		number of points in time series
!	tbp		the time-bandwidth product
!	kspec   	number of tapers to use
!	nf		number of freq points (npts/2+1)
!       lambda(kspec)   the eigenvalues of the Slepian sequences
!	vn(npts,kspec)  the slepian sequences
!       yk(npts,kspec)  multitaper eigencoefficients, complex
!	wt(nf,kspec)	the weights of the different coefficients. 
!			input is the original multitaper weights, 
!			from the Thomson adaptive weighting. 
!	
!  IN/OUT
!
!	qispec(nf)	the QI spectrum
!			in input is the multitaper spectrum, 
!			in output is the unbiased Quadratic 
!			spectrum.
!
!  OUTPUT
!
!	slope(nf)	the estimate of the first derivative
!
!  MODIFIED
!	German Prieto
!	June 5, 2009
!		Major change, saving some important
!		values so that if the subroutine is called 
!		more than once, with similar values, many of
!		the variables are not calculated again, making
!		the code run much faster. 
!
!********************************************************************

   implicit none

!  Odd freq points in 2W bandwidth

   integer, parameter                     :: nxi = 79
   integer, parameter                     :: nal = 3, ntot=5 

!  Input

   integer,                           intent(in)   :: kspec, npts,  nf
   real(8),                           intent(in)   :: tbp
   complex(8), dimension(npts,kspec), intent(in)   :: yk 
   real(8),    dimension(kspec),      intent(in)   :: lambda
   real(8),    dimension(npts,kspec), intent(in)   :: vn
   real(8),    dimension(nf,kspec),   intent(in)   :: wt


!  In/Out & Out

   real(8), dimension(nf),   intent(in out)        :: qispec  !*****

!  NS Output

   real(8), dimension(nf), intent(out)         :: qislope, qiquad
   real(8), dimension(nf), intent(out)         :: nsdiff, nsd2
   real(8), dimension(nf), intent(out)         :: cte_var, slope_var
   real(8), dimension(nf), intent(out)         :: nscte_var, nsslope_var
   real(8), dimension(nf), intent(out)         :: qiquad_var, nsquad_var

!  Frequency Resampling

   real(8)                                :: bp, w
   real(8)                                :: dxi, ct, st, om
   real(8),    dimension(nxi)             :: rxi
   real(8),    dimension(nxi)             :: xi
   real(8),    dimension(npts)            :: vn_dpss

!  Spectrum parameters

   complex(8), dimension(nf,kspec)        :: xk

!  Covariance and Projection matrices

   complex(8), dimension(kspec*kspec,nf)  :: C
   complex(8), dimension(nxi,kspec)       :: Vj 
   complex(8), dimension(kspec*kspec,nxi) :: Pk 

!  The Quadratic model

   real(8),    dimension(nf)              :: slope
   real(8),    dimension(nf)              :: sigma2
   real(8),    dimension(nxi)             :: hcte, hslope, hquad
   complex(8), dimension(3)               :: hmodel
   complex(8), dimension(ntot)            :: ft_hmodel

!  Sinc2 matrix

   real(8), dimension(npts,npts) :: Ksinc

!  Al eigenfunctions

   real(8), dimension(npts,nal)  :: A
   real(8), dimension(nal)       :: hl
   real(8)                       :: scl

!  The NS Quadratic model

   complex(8), dimension(nal)             :: qmodel

!  QR Decomposition

   complex(8), dimension(ntot)              :: ft_btilde

!  Others

   integer                                :: nh, n
   integer                                :: L, i, j, k, m 
   integer                                :: ic

   real(8), parameter :: pi=3.141592653589793d0, tpi = 2.d0*pi

!  Save values

   integer, save                              :: npts_2, kspec_2
   real(8), save                              :: tbp_2

!  Ql, and QR matrices to save

   complex(8), dimension(:,:), allocatable, save     :: hk 
   real(8),    dimension(:,:), allocatable, save     :: Qvec 

   complex(8), dimension(:,:), allocatable, save  :: ft_H 
   complex(8), dimension(:,:), allocatable, save  :: ft_Q
   complex(8), dimension(:,:), allocatable, save  :: ft_Q_T
   complex(8), dimension(:,:), allocatable, save  :: ft_R
   real(8),    dimension(:,:), allocatable, save  :: ft_cov

!********************************************************************

   if (minval(lambda)<0.9d0) then
      write(6,*) 'Careful, Poor leakage of eigenvalue ', minval(lambda)
      write(6,*) 'Value of kspec is too large, revise? *****'
      !stop
   endif

   do k = 1,kspec
      xk(:,k) = wt(:,k)*yk(1:nf,k)
   enddo


!  New inner bandwidth frequency
!  Many of this is double precision.

   ic  = kspec*kspec
   n   = nxi  
   nh  = 3 
   bp  = tbp/(dble(npts))               ! W bandwidth
   w   = tbp / dble(npts)

   if (kspec/=kspec_2 .or. npts_2/=npts .or. tbp_2/=tbp ) then

      if (allocated(hk)) then
         deallocate(hk, Qvec, ft_H)
         deallocate(ft_Q, ft_Q_T,ft_cov,ft_R)
      endif
 
      allocate(hk(kspec*kspec,3)) 
      allocate(Qvec(ic,nal))

      allocate(ft_H(ic,ntot))
      allocate(ft_Q(ic,ntot))
      allocate(ft_Q_T(ntot,ic))
      allocate(ft_cov(ntot,ntot))
      allocate(ft_R(ntot,ntot))

      dxi = (2.d0*dble(bp))/dble(nxi-1)   ! QI freq. sampling

      xi  = dble( (/(i-(nxi/2)-1, i=1,nxi)/) )*dxi

      do j = 1,kspec
         vn_dpss = vn(:,j)

         do i = 1,nxi

            om = tpi* xi(i)     ! Normalize to unit sampling, for FFT
       
            call sft(vn_dpss,npts,om,ct,st)

            Vj(i,j)  = 1.d0/sqrt(lambda(j)) * cmplx(ct,st,kind=8)

         enddo
      enddo

!
!  Create the vectorized Pk matrix {Vj Vk*}
!

      L = kspec*kspec

      m = 0 
      do j = 1,kspec
         do k = 1,kspec

            m = m + 1

            Pk(m,1:nxi) = conjg(Vj(:,j)) * (Vj(:,k))

         enddo
      enddo

      Pk(1:m,1)         = 0.5d0 * Pk(1:m,1)
      Pk(1:m,nxi)       = 0.5d0 * Pk(1:m,nxi)

!  I use the Chebyshev Polynomial as the expansion basis.

      rxi = dble( (/(i-(nxi/2)-1, i=1,nxi)/) ) / dble(nxi/2) * bp
 
      hcte(1:nxi)  = 1.d0 
      hk(:,1) = matmul(Pk,hcte) * dxi 

      hslope(1:nxi) = rxi/bp 
      hk(:,2) = matmul(Pk,hslope) * dxi 

      hquad(1:nxi) = (2.d0*((rxi/bp)**2) - 1.0d0)
      hk(:,3) = matmul(Pk,hquad) * dxi 

      if (m .ne. L) then 
         write(6,*) 'Error in matrix sizes, stopped '
         stop
      endif 

!  THE NON-STATIONARY PART

      do n = 1,npts
         do m = 1,npts
      
            if (n==m) then
               Ksinc(n,m) = (2.d0*w)**2;
            else   
               Ksinc(n,m) = ( sin(2.d0*pi*w*dble(n-m))/(pi*dble(n-m)) )**2
            endif

         enddo
      enddo

      Ksinc = dble(npts) * Ksinc

      call rsm_eig( npts, Ksinc, nal, hl, A)

!     Normalize so (1/N) sum from 1 to npts A(n,k)**2 = 1

      do i = 1,nal
            scl    = sum( A(:,i) * A(:,i) )
            A(:,i) = A(:,i) * sqrt( dble(npts)/scl )
      enddo
 
!     Put positive standard
 
      do i = 1,nal
         if (A((npts)/2 + 1,i)-A((npts)/2,i) < 0.d0 ) then
            A(:,i) = -A(:,i)
         endif
      enddo

! Test with Chebyshev

      A(:,2) = dble( (/(i-(npts/2)-1, i=1,npts)/) ) / dble(npts/2)
 
      A(:,1)  = 1.d0 
      A(:,3) = (2.d0*((A(:,2))**2) - 1.0d0)

!
!     Create the vectorized Ql matrix 
!     sqrt(lambda(j)*lambda(k)) {vj vk*} A
!

      Qvec  = 0.d0
      m = 0 
      do j = 1,kspec
         do k = 1,kspec

            m = m + 1

            do n = 1,npts
               Qvec(m,:)  = Qvec(m,:)                 &
                                + sqrt(lambda(j)*lambda(k))        &
                                * (  (vn(n,j)*vn(n,k)) * A(n,:) )
            enddo

         enddo
      enddo

!  Do both together, frequency and time domains

      ft_H(:,1:3) = hk
      ft_H(:,4:5) = Qvec(:,2:3)

!  QR decomposition
 
      call zqrfac(m,ntot,ft_H,ft_Q,ft_R)
      call zqrcov(ntot,ntot,ft_R(1:ntot,1:ntot),ft_cov)

!
!  The least squares solution for the alpha vector
!

      ft_Q_T = transpose(ft_Q)
      ft_Q_T = conjg(ft_Q_T)

      print *, real(ft_cov(:,1))
      print *, real(ft_cov(:,2))
      print *, real(ft_cov(:,3))
      print *, real(ft_cov(:,4))
      print *, real(ft_cov(:,5))

      npts_2  = npts
      kspec_2 = kspec
      tbp_2   = tbp

   endif


!
!  Create the vectorized Cjk matrix 
!

   L = kspec*kspec

   m = 0 
   do j = 1,kspec
      do k = 1,kspec

         m = m + 1
         C(m,:) = ( conjg(xk(:,j)) * (xk(:,k)) )

      enddo
   enddo

   if (m .ne. L) then 
      write(6,*) 'Error in matrix sizes, stopped '
      stop
   endif 

!  Remove the predicted values for the spectrum input

   do j = 1, nf

!     Put results for standard MT spectrum

      ft_hmodel      = 0.d0
      ft_hmodel(1) = qispec(j)

      sigma2(j) = sum(abs( C(:,j) - matmul(ft_H,real(ft_hmodel)) )**2)/dble(L-ntot)

!     Don´t allow variance to be smaller than real(4) limit (set to 1e-6)
      if (sigma2(j) < 1.0d-6) then
         sigma2(j) = 1.0d-6
      endif

      cte_var(j)     = sigma2(j)*ft_cov(1,1)

      C(:,j) = C(:,j) - matmul(ft_H,real(ft_hmodel))
       
   enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---
! THE STATIONARY/NONSTATIONARY LEAST SQUARES
!---
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!---
! Begin Least squares
!---

!
!  The least squares solution for the alpha vector
!

   do j = 1, nf

!     The back transformation (for complex variable)

      ft_btilde = matmul(ft_Q_T,C(:,j))

      ft_hmodel = 0.d0

      do i = ntot, 1, -1
         do k = i+1,ntot 
            ft_hmodel(i) = ft_hmodel(i) - ft_R(i,k)*ft_hmodel(k)
         enddo
         ft_hmodel(i) = (ft_hmodel(i) + ft_btilde(i))/ft_R(i,i)
      enddo

!      qispec(j)  = real(ft_hmodel(1))    
      qislope(j) = real(-ft_hmodel(2))    ! Get correct units (it is switched)
      qiquad(j)  = real(ft_hmodel(3))
      nsdiff(j)  = real(ft_hmodel(4))    
      nsd2(j)    = real(ft_hmodel(5))

      sigma2(j) = sum(abs( C(:,j) - matmul(ft_H,real(ft_hmodel)) )**2)/dble(L-ntot)

!     Don´t allow variance to be smaller than real(4) limit (set to 1e-6)
      if (sigma2(j) < 1.0d-6) then
         sigma2(j) = 1.0d-6
      endif

!      cte_var(j)     = sigma2(j)*ft_cov(1,1)
      slope_var(j)   = sigma2(j)*ft_cov(2,2)
      qiquad_var(j)    = sigma2(j)*ft_cov(3,3)
      nsslope_var(j) = sigma2(j)*ft_cov(4,4)
      nsquad_var(j)  = sigma2(j)*ft_cov(5,5)

   enddo

!!
!!  Put slope and curvature in the correct units
!!
!
!   qislope = qislope / (bp)
!   quad  = quad  / (bp**2)
!
!   slope_var = slope_var / (bp**2)
!   quad_var = quad_var / (bp**4)
!
!!
!!  Compute the Quadratic Multitaper
!!  Eq. 33 and 34 of Prieto et. al. (2007)
!!
!
!   do i = 1,nf
!      qispec(i) = qispec(i) - (quad(i)**2)/((quad(i)**2) + quad_var(i) )  &
!                        * (1.d0/6.d0)*(bp**2)*quad(i)
!   enddo


!---
! Finished Least squares
!---


end subroutine qi_nsqi3





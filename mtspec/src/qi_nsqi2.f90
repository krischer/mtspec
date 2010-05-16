subroutine qi_nsqi2(npts,tbp,kspec,nf,lambda,vn,yk,wt,  &
                 qispec,qislope,qiquad,nsspec,nsdiff,nsd2,    &
                 cte_var,slope_var,nscte_var,nsslope_var,A)

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
   integer, parameter                     :: nal = 3

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
   real(8), dimension(nf), intent(out)         :: nsspec, nsdiff, nsd2
   real(8), dimension(nf), intent(out)         :: cte_var, slope_var
   real(8), dimension(nf), intent(out)         :: nscte_var, nsslope_var

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

   real(8),    dimension(nf)              :: cte, slope, quad
   real(8),    dimension(nf)              :: quad_var, nsquad_var, sigma2
   real(8),    dimension(nxi)             :: hcte, hslope, hquad
   complex(8), dimension(3)               :: hmodel

!  Sinc2 matrix

   real(8), dimension(npts,npts) :: Ksinc

!  Al eigenfunctions

   real(8), dimension(npts,nal)  :: A
   real(8), dimension(nal)       :: hl
   real(8)                       :: scl

!  The NS Quadratic model

   complex(8), dimension(nal)             :: qmodel

!  QR Decomposition

   complex(8), dimension(3)              :: btilde
   complex(8), dimension(nal)            :: nsbtilde

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
   complex(8), dimension(:,:), allocatable, save     :: Q
   complex(8), dimension(:,:), allocatable, save     :: Q_T
   complex(8), dimension(:,:), allocatable, save     :: R
   real(8),    dimension(:,:), allocatable, save     :: cov

   real(8), dimension(:,:), allocatable, save     :: Qvec 
   real(8), dimension(:,:), allocatable, save     :: nsQ
   real(8), dimension(:,:), allocatable, save     :: nsQ_T
   real(8), dimension(:,:), allocatable, save     :: nsR
   real(8), dimension(:,:), allocatable, save     :: nscov

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
         deallocate(hk, Q, Q_T, cov, R)
         deallocate(Qvec,nsQ, nsQ_T,nscov,nsR)
      endif
 
      allocate(hk(kspec*kspec,3)) 
      allocate(Q(kspec*kspec,3)) 
      allocate(R(3,3),cov(3,3))
      allocate(Q_T(3,kspec*kspec))  

      allocate(Qvec(ic,nal))
      allocate(nsQ(ic,nal))
      allocate(nsQ_T(nal,ic))
      allocate(nscov(nal,nal))
      allocate(nsR(nal,nal))

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

!  QR decomposition
 
      call zqrfac(m,nh,hk,Q,R)
      call zqrcov(nh,nh,R(1:nh,1:nh),cov)

!
!  The least squares solution for the alpha vector
!

      Q_T = transpose(Q)
      Q_T = conjg(Q_T)

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

!     QR decomposition

      call qrfac8(m,nal,Qvec,nsQ,nsR)
      call qrcov(nal,nal,nsR(1:nal,1:nal),nscov)

      nsQ_T = transpose(nsQ)

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---
! THE STATIONARY LEAST SQUARES
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

      btilde = matmul(Q_T,C(:,j))

      hmodel = 0.d0

      do i = nh, 1, -1
         do k = i+1, nh
            hmodel(i) = hmodel(i) - R(i,k)*hmodel(k)
         enddo
         hmodel(i) = (hmodel(i) + btilde(i))/R(i,i)
      enddo

      cte(j)   = real(hmodel(1))    
      qislope(j) = real(-hmodel(2))    ! Get correct units (it is switched)
      qiquad(j)  = real(hmodel(3))

      sigma2(j) = sum(abs( C(:,j) - matmul(hk,real(hmodel)) )**2)/dble(L-nh)

      cte_var(j)   = sigma2(j)*cov(1,1)
      slope_var(j) = sigma2(j)*cov(2,2)
      quad_var(j)  = sigma2(j)*cov(3,3)

   enddo

   qispec = cte 

!---
! Finished Least squares
!---

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---
! NON STATIONARY LEAST SQUARES
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

      nsbtilde = matmul(nsQ_T,C(:,j))

      qmodel = 0.d0

      do i = nal, 1, -1
         do k = i+1, nal
            qmodel(i) = qmodel(i) - nsR(i,k)*qmodel(k)
         enddo
         qmodel(i) = (qmodel(i) + nsbtilde(i))/nsR(i,i)
      enddo

      cte(j)   = (qmodel(1))    
      slope(j) = (qmodel(2))    
      quad(j)  = (qmodel(3))

      sigma2(j) = sum(abs( C(:,j) - matmul(Qvec,real(qmodel)) )**2)/dble(L-nal)

      nscte_var(j)   = sigma2(j)*nscov(1,1)
      nsslope_var(j) = sigma2(j)*nscov(2,2)
      nsquad_var(j)  = sigma2(j)*nscov(3,3)

   enddo

   nsspec = real(cte)
   nsdiff = real(slope)
   nsd2   = real(quad)

end subroutine qi_nsqi2






subroutine qiinv(npts,tbp,kspec,nf,lambda,vn,yk,wt,  &
                 qispec,slope)

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
!********************************************************************

   use spectra

   implicit none

!  Odd freq points in 2W bandwidth

   integer, parameter                     :: nxi = 79

!  Input

   integer,                           intent(in)   :: kspec, npts,  nf
   real(8),                           intent(in)   :: tbp
   complex(8), dimension(npts,kspec), intent(in)   :: yk 
   real(8),    dimension(kspec),      intent(in)   :: lambda
   real(8),    dimension(npts,kspec), intent(in)   :: vn
   real(8),    dimension(nf,kspec),   intent(in)   :: wt


!  In/Out & Out

   real(8), dimension(nf),   intent(in out)        :: qispec  !*****
   real(8), dimension(nf),   intent(out)           :: slope

!  Frequency Resampling

   real(8)                                :: bp, df
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

   real(8),    dimension(nf)              :: cte, quad
   real(8),    dimension(nf)              :: cte_var, slope_var
   real(8),    dimension(nf)              :: quad_var, sigma2, freq
   real(8),    dimension(nxi)             :: hcte, hslope, hquad
   complex(8), dimension(3)               :: hmodel
   complex(8), dimension(kspec*kspec,3)   :: hk 

!  QR Decomposition

   complex(8), dimension(3)              :: btilde
   complex(8), dimension(kspec*kspec,3)  :: Q
   complex(8), dimension(3,3)            :: R
   complex(8), dimension(3,kspec*kspec)  :: Q_T

   real(8), dimension(3,3)               :: cov


!  Others

   integer                                :: nh, n
   integer                                :: L, i, j, k, m 

   real(8), parameter :: pi=3.141592653589793d0, tpi = 2.d0*pi

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

   bp = tbp/(dble(npts))		! W bandwidth

   dxi = (2.d0*dble(bp))/dble(nxi-1)	! QI freq. sampling

   df = 0.5d0/dble(nf - 1)                ! Assume unit sampling

   xi  = dble( (/(i-(nxi/2)-1, i=1,nxi)/) )*dxi

   do j = 1,kspec
      vn_dpss = vn(:,j)

      do i = 1,nxi

         om = tpi* xi(i) 	! Normalize to unit sampling, for FFT
       
         call sft(vn_dpss,npts,om,ct,st)

         Vj(i,j)  = 1.d0/sqrt(lambda(j)) * cmplx(ct,st,kind=8)

     enddo
   enddo

!
!  Create the vectorized Cjk matrix and Pk matrix {Vj Vk*}
!

   L = kspec*kspec

   m = 0 
   do j = 1,kspec
      do k = 1,kspec

         m = m + 1
         C(m,:) = ( conjg(xk(:,j)) * (xk(:,k)) )

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
   n = nxi  
   nh = 3 

!---
! Begin Least squares
!---

!  QR decomposition
 
   call zqrfac(m,nh,hk,Q,R)
   call zqrcov(nh,nh,R(1:nh,1:nh),cov)

   print *, cov

!
!  The least squares solution for the alpha vector
!

   Q_T = transpose(Q)
   Q_T = conjg(Q_T)

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
      slope(j) = real(-hmodel(2))    ! Get correct units (it is switched)
      quad(j)  = real(hmodel(3))

      sigma2(j) = sum(abs( C(:,j) - matmul(hk,real(hmodel)) )**2)/dble(L-nh)

      cte_var(j)   = sigma2(j)*cov(1,1)
      slope_var(j) = sigma2(j)*cov(2,2)
      quad_var(j)  = sigma2(j)*cov(3,3)

   enddo

!---
! Finished Least squares
!---

   freq = dble( (/(i-1, i=1,nf)/) ) * dble(df)

!
!  Put slope and curvature in the correct units
!

   slope = slope / (bp)
   quad  = quad  / (bp**2)

   slope_var = slope_var / (bp**2)
   quad_var = quad_var / (bp**4)

!
!  Compute the Quadratic Multitaper
!  Eq. 33 and 34 of Prieto et. al. (2007)
!

   do i = 1,nf
      qispec(i) = qispec(i) - (quad(i)**2)/((quad(i)**2) + quad_var(i) )  &
                        * (1.d0/6.d0)*(bp**2)*quad(i)
   enddo

!   open(12,file='est_deriv.dat')
!   do i = 1,nf
!      write(12,*) freq(i), qispec(i), slope(i), quad(i)
!   enddo
!   close(12)

end subroutine qiinv



subroutine qins(npts,tbp,kspec,nf,lambda,vn,yk,wt,  &
                 nsspec,ns_slope,ns_quad)

!
!  Calculate the Non-Stationary Inverse Theory Spectrum.
!  Basically, compute the spectrum inside the innerband, 
!  and its time derivative
!
!  This approach is very similar to 
!  D.J. Thomson (1990).
!  
!  and
!
!  An overview of multiple-window and Quadratic-inverse 
!  Spectrum Estimation Methods.
!  David J. Thomson
!  1994
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
!	nsspec(nf)	the QI spectrum
!			in input is the multitaper spectrum, 
!			in output is the unbiased Quadratic 
!			spectrum.
!
!  OUTPUT
!
!	ns_slope(nf)	the estimate of the first derivative
!       ns_quad(nf)     the estimate of the second derivative
!
!********************************************************************

   use spectra
   use plot

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

   real(8), dimension(nf),   intent(in out)        :: nsspec  !*****
   real(8), dimension(nf),   intent(out)           :: ns_slope
   real(8), dimension(nf),   intent(out)           :: ns_quad

!  Frequency Resampling

   real(8)                                :: bp, df
   real(8)                                :: dxi, ct, st, om
   real(8),    dimension(npts)            :: rnpts
   real(8),    dimension(npts)            :: xi
   real(8),    dimension(npts)            :: vn_dpss

!  Spectrum parameters

   complex(8), dimension(nf,kspec)        :: xk

!  Covariance and Projection matrices

   complex(8), dimension(kspec*kspec,nf)  :: C
   complex(8), dimension(kspec*kspec,npts) :: Tk 

!  The Quadratic model

   real(8),    dimension(nf)              :: cte, quad
   real(8),    dimension(nf)              :: cte_var, slope_var
   real(8),    dimension(nf)              :: quad_var, sigma2, freq
   real(8),    dimension(npts)            :: hcte, hslope, hquad
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

!
! Create the weighted xk's
!

   do k = 1,kspec
      xk(:,k) = wt(:,k)*yk(1:nf,k)
   enddo


!  New inner bandwidth frequency
!  Many of this is double precision.

!
!  Create the vectorized Cjk matrix and Theta_l matrix {vn vm}
!

   L = kspec*kspec

   m = 0 
   do j = 1,kspec
      do k = 1,kspec

         m = m + 1
         C(m,:) = ( conjg(xk(:,j)) * (xk(:,k)) )

         Tk(m,1:npts) = sqrt(lambda(j)*lambda(k)) * (vn(:,j)) * (vn(:,k))

      enddo
   enddo

!  I use the Chebyshev Polynomial as the expansion basis.

   rnpts = dble( (/(i-(npts/2)-1, i=1,npts)/) ) / dble(npts/2) 
 
   hcte(1:npts)  = 1.d0 
   hk(:,1) = matmul(Tk,hcte) * dxi 

   hslope(1:npts) = rnpts/bp 
   hk(:,2) = matmul(Tk,hslope) * dxi 

   hquad(1:npts) = (2.d0*((rnpts/bp)**2) - 1.0d0)
   hk(:,3) = matmul(Tk,hquad) * dxi 


   if (m .ne. L) then 
      write(6,*) 'Error in matrix sizes, stopped '
      stop
   endif 
   n = nxi  
   nh = 3 

   call gplot(real(hcte),'hold')
   call gplot(real(hslope),'hold')
   call gplot(real(hquad))

   stop

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

      cte(j)      = real(hmodel(1))    
      ns_slope(j) = real(-hmodel(2))    ! Get correct units (it is switched)
      ns_quad(j)  = real(hmodel(3))

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

   ns_slope = ns_slope / (bp)
   ns_quad  = ns_quad  / (bp**2)

   slope_var = slope_var / (bp**2)
   quad_var = quad_var / (bp**4)

!
!  Compute the Quadratic Multitaper
!  Eq. 33 and 34 of Prieto et. al. (2007)
!
!
!   do i = 1,nf
!      qispec(i) = qispec(i) - (quad(i)**2)/((quad(i)**2) + quad_var(i) )  &
!                        * (1.d0/6.d0)*(bp**2)*quad(i)
!   enddo
!
!   open(12,file='est_deriv.dat')
!   do i = 1,nf
!      write(12,*) freq(i), qispec(i), slope(i), quad(i)
!   enddo
!   close(12)

end subroutine qins



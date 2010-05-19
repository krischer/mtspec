subroutine qi_line(npts,tbp,kspec,nf,df,freq,lambda,vn,  &
                   fcritical,pin)!,q0line,q1line)

!
!  Calculate the Stationary Inverse Theory Spectrum.
!  Basically, compute the spectrum inside the innerband. 
!  Estimate the amplitude of a line component, assuming 
!  there is a single line inside the bandwidth, with a 
!  frequency center on our particular frequency bin. 
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
!	df		frequency sampling
!	freq(nf)	the frequency vector
!       lambda(kspec)   the eigenvalues of the Slepian sequences
!	vn(npts,kspec)  the slepian sequences
!	fcritical	the F-test critical value
!	pin(nf)		the F-test probability 
!			(has to be > fcritical to be used)
!	
!  OUT
!
!	q0line(nf)	the QI spectrum for the lines in sline
!			in input is the multitaper spectrum, 
!			in output is the unbiased Quadratic 
!			spectrum.
!	q1line(nf)	the estimate of the first derivative
!
!********************************************************************

   use spectra
   use plot

   implicit none

!  Odd freq points in 2W bandwidth

   integer, parameter                     :: nxi = 20 

!  Input

   integer,                           intent(in)   :: kspec, npts,  nf
   real(8),                           intent(in)   :: tbp, df
   real(4),    dimension(nf),         intent(in)   :: freq
   real(8),    dimension(kspec),      intent(in)   :: lambda
   real(8),    dimension(npts,kspec), intent(in)   :: vn
   real(4),                           intent(in)   :: fcritical
   real(4),    dimension(nf),         intent(in)   :: pin


!  Out

!   real(8), dimension(nf),   intent(in out)        :: q0line  !*****
!   real(8), dimension(nf),   intent(out)           :: q1line

!  Frequency Resampling

   real(8)                                :: bp
   real(8)                                :: dxi, ct, st, om
   real(8),    dimension(nf)		  :: freq8
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
   real(8),    dimension(nf)              :: quad_var, sigma2
   real(8),    dimension(nxi)             :: hcte, hslope, hquad
   complex(8), dimension(3)               :: hmodel
   complex(8), dimension(kspec*kspec,3)   :: hk 

!  QR Decomposition

   complex(8), dimension(kspec*kspec)     :: btilde
   complex(8), dimension(kspec*kspec,nxi) :: Q
   complex(8), dimension(nxi,nxi)         :: R
   complex(8), dimension(nxi,kspec*kspec) :: Q_T

   real(8), dimension(3,3)               :: cov


!  Others

   integer                                :: nh, n
   integer                                :: L, i, j, k, m 

   real(8), parameter :: pi=3.141592653589793d0, tpi = 2.d0*pi

!********************************************************************

!  Create real(8) frequency vector
 
   print *, ' Starting'

   freq8 = dble(freq)

   bp = tbp/(dble(npts))		! W bandwidth

   print *, 'Step 2'
!  Find how many points inside (-W,W), go one more point is necessary

   do i = 1,2*kspec	! just a large number
      if ( freq8(i) > freq8(i)+bp ) then  
         L = i
         exit
      endif
   enddo

   print *, 'Step 3'

   j = 0
   do i = -L,L
      j = j + 1
      xi(j) = 0.d0 + dble(L)*df
   enddo

   print *, xi
   return 




   dxi = (2.d0*dble(bp))/dble(nxi-1)	! QI freq. sampling

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

!   hquad(1:nxi) = 0.5d0 * (3.d0*((rxi/bp)**2) - 1.0d0)  
   hquad(1:nxi) = (2.d0*((rxi/bp)**2) - 1.0d0)
   hk(:,3) = matmul(Pk,hquad) * dxi 


   if (m .ne. L) then 
      write(6,*) 'Error in matrix sizes, stopped '
      stop
   endif 
   n = nxi  
   nh = 3 

!     QR decomposition
 
   call zqrfac(m,n,hk,Q,R)
   call zqrcov(nh,nh,R(1:nh,1:nh),cov)

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
      !slope(j) = real(-hmodel(2))    ! Get correct units (it is switched)
      quad(j)  = real(hmodel(3))

      sigma2(j) = sum(abs( C(:,i) - matmul(hk,real(hmodel)) )**2)/dble(L-nh)

      cte_var(j)   = sigma2(j)*cov(1,1)
      slope_var(j) = sigma2(j)*cov(2,2)
      quad_var(j)  = sigma2(j)*cov(3,3)

   enddo

   !slope = slope / (bp)
   !quad  = quad  / (bp**2)

   !slope_var = slope_var / (bp**2)
   !quad_var = quad_var / (bp**4)

   !do i = 1,nf
      !qispec(i) = qispec(i) - (quad(i)**2)/((quad(i)**2) + quad_var(i) )  &
      !                  * (1.d0/6.d0)*(bp**2)*quad(i)
   !enddo

end subroutine qi_line



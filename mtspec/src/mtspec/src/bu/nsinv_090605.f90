subroutine nsinv(npts,dt,x,tbp,kspec,nf,itype,jump,freq,spec,D,cte,slope)

!
!  Calculate the Non-stationary Inverse Theory Spectrum.
!  Basically, compute the spectrum and the Power of the 
!  signal. 
!
!  This approach is very similar to 
!  J.J. Park (1992) 
!  D.J. Thomson (1994).
!
!  For each frequency an envelope squared (Power) as a 
!  function of time is estimated, which can be used to 
!  create a better spectrogram, or simply look how the
!  spectrum is behaving inside the window of interest. 
!
!
!  INPUT
!
!	npts		number of points in time series
!	dt		sampling rate
!	x(npts)`	the time series, real
!	tbp		the time-bandwidth product
!	kspec		number of tapers to use
!	nf		number of freq points (npts/2+1)
!       itype		0 - min 2norm, 1 - min derivative
!			2 - min smoothness
!	jump		point where to jump in smoothness
!
!  OUTPUT
!
!	freq(nf)	frequency vector
!	spec(nf)	the spectrum estimate
!	D(npts,nf)	the power of the signal
!       cte		the constant estimate
!       slope 		slope or time derivative
!
!********************************************************************

   use spectra

   implicit none

!  Input

   integer, intent(in)         :: kspec, npts,  nf, itype, jump
   real(4), intent(in)                  :: dt, tbp
   real(4), dimension(npts), intent(in) :: x

!  Out

   real(4), dimension(nf),      intent(out) :: freq, spec
   real(4), dimension(npts,nf), intent(out) :: D
   real(4), dimension(nf),      intent(out) :: cte, slope

!  Spectrum parameters

   real(4), dimension(nf)       :: spec2

   complex(4), dimension(npts,kspec) ::  yk 
   real(4), dimension(nf,kspec) :: wt
   complex(4), dimension(nf,kspec) :: xk

   real(4) :: sscal, sscal2

!  Dpss 

   real(8), dimension(kspec)      :: lambda
   real(8), dimension(npts,kspec) :: vn

!  Covariance

   real(4), dimension(:,:), allocatable :: C, G, Ch, hk
   real(4), dimension(:),   allocatable :: model, hmodel
   real(4), dimension(npts)             :: hcte, hslope

!  Derivatives smoothing

   real(4), dimension(:,:), allocatable :: delta

   real(4) :: mu 

!  QR Decomposition

   real(4), allocatable, dimension(:,:) :: Q, R, Q_T
   real(4), allocatable, dimension(:)   :: btilde

!  Others

   integer :: L, i, j, k, m, n, nh

!********************************************************************

   if (itype > 2 .or. itype < 0) then
      write(6,*) 'Wrong type of solution, itype = ', itype
      stop
   endif

!  Get the spectrum estimate

   call mtspec(npts,dt,x,tbp,kspec,nf,freq,spec,   &
                yk=yk, wt=wt )

!   call mtspec(npts,dt,x,tbp,kspec,nf,freq,spec,   &
!                yk=yk, wt=wt, vn=vn,lambda=lambda  )

   
!
! Create the weigthed xk = dk*yk
!

   do k = 1,kspec
      xk(:,k) = wt(:,k)*yk(1:nf,k)
   enddo

   spec2 = sum((abs(xk)**2),dim=2)/real(kspec) !/sum(wt**2,dim=2) !real(kspec) 

!
!  scale xk to meet parseval's theorem, by scaling to spectrum
!

   sscal  = 0.5 * (spec(1)  + spec(nf) )
   sscal2 = 0.5 * (spec2(1) + spec2(nf))
   do i=2, nf-1
      sscal  = sscal  + spec(i)
      sscal2 = sscal2 + spec2(i)
   enddo

   yk = yk * sqrt(sscal/sscal2)

   do k = 1,kspec
      !xk(:,k) = yk(1:nf,k)
      xk(:,k) = wt(:,k)*yk(1:nf,k)
   enddo

   spec2 = sum((abs(xk)**2),dim=2)/sum(wt**2,dim=2) !real(kspec) 

!
!  Create the vectorized Cjk matrix
!  and the G = {vj vk} matrix
!  Note that because the system is underdetermined and rank
!  deficient, we need to use a damping parameter to solve 
!  the least squares problem. 
!

   L = kspec*(kspec)

   allocate(C(L+npts,nf))
   allocate(hk(L,2))
   allocate(G(L+npts,npts))
   allocate(model(npts))
   allocate(delta(npts,npts))

   m = 0
   do j = 1,kspec
      do k = 1,kspec

         m = m + 1
         C(m,:) = real( (xk(:,j)) * conjg(xk(:,k)) )

         G(m,:) = real(sqrt(lambda(j)*lambda(k)) *  vn(:,j)*vn(:,k))

      enddo
   enddo

!  Prefered model (constant + slope)
!  model    D = h + r
!  min || r || = min || D - bh ||

   hcte(1:npts)  = 1.*dt
   hslope(1:npts) = real( (/(i-1, i=1,npts)/) )*dt

   hk(:,1) = matmul(G(1:L,1:npts),hcte)
   hk(:,2) = matmul(G(1:L,1:npts),hslope)

!  Allocate QR variables

   m = L
   n = npts
   nh = 2
   
   allocate(Ch(m,nf))
   allocate(btilde(m))
   allocate(Q(m,n))
   allocate(Q_T(n,m))
   allocate(R(n,n))
   allocate(hmodel(nh))

   Ch = C(1:L,:)

!  QR decomposition

   call qrfac(m,n,hk,Q,R)

   Q_T = transpose(Q)

   do j = 1,nf

      btilde = matmul(Q_T,Ch(:,j))

      hmodel = 0.0

      do i = nh, 1, -1
         do k = i+1, nh
            hmodel(i) = hmodel(i) - R(i,k)*hmodel(k)
         enddo
         hmodel(i) = (hmodel(i) + btilde(i))/R(i,i)
      enddo

      cte(j)   = hmodel(1)
      slope(j) = hmodel(2)

      C(1:L,j) = C(1:L,j) - hk(1:L,1)*cte(j) - hk(1:L,2)*slope(j)

   enddo

   deallocate(btilde)
   deallocate(Q)
   deallocate(Q_T)
   deallocate(R)

!  Add the derivative matrices.

   delta = 0.

   if (itype == 1) then 
      delta(1,1) = 0. 
      do i = 2,npts
         delta(i,i) = 1.
         delta(i,i-1) = -1.
      enddo
      delta = delta/real(npts) 

   elseif (itype == 2) then

      print *, 'Second derivative ', itype
      delta(1,1) = 0. 
      do i = 2,npts-1
         delta(i,i)   = 2.
         delta(i,i-1) = -1.
         delta(i,i+1) = -1.
      enddo
      delta(npts,npts) = 0.
      delta = delta/real(npts)

   else 

      print *, 'Minimum norm ', itype
      do i = 1,npts
         delta(i,i) = 1.
      enddo
      delta = delta/real(npts)

   endif

!  Allow jump in derivative norm
!  2 norm is not allowed to jump

   if (itype > 0 .and. jump /= 0 .and. jump<npts) then
      print *, 'Jump ', jump
      G(m+jump,:) = 0.
   endif

   C(L+1:L+npts,:) = 0.


   m = L + npts
   n = npts 

!  QR decomposition

   allocate(btilde(m))
   allocate(Q(m,n))
   allocate(Q_T(n,m))
   allocate(R(n,n))

!
!  Loop over mu values (regularization)
!

   mu = 10000.
   G(L+1:L+npts,:) = delta * sqrt(mu)

   call qrfac(m,n,G,Q,R)

   Q_T = transpose(Q)

   do j = 1,nf

      btilde = matmul(Q_T,C(:,j))

      model = 0.0

      do i = n, 1, -1
         do k = i+1, n
            model(i) = model(i) - R(i,k)*model(k)
         enddo
         model(i) = (model(i) + btilde(i))/R(i,i)
      enddo

      D(:,j) = model(1:npts) + hcte*cte(j) + hslope*slope(j)
!      D(:,j) = hcte*cte(j) + hslope*slope(j)

   enddo

   deallocate(C, G, Ch, hk, model, hmodel, delta)
   deallocate(Q, R, Q_T, btilde)

end subroutine nsinv

!--------------------------------------------------------------------







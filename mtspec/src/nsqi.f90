subroutine nsqi(npts,tbp,kspec,nf,lambda,vn,yk,wt,   &
                nsspec,nsdiff,nsd2)

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
!  In here I use the eigenvalues/functions of the SINC2 kernel
!  since these are appropiate for orthogonal basis matrices. 
!

!********************************************************************

   implicit none

!  Input

   integer, intent(in)  :: npts,kspec,nf

   real(8),                           intent(in) :: tbp
   real(8),    dimension(kspec),      intent(in) :: lambda 

   real(8),    dimension(npts,kspec), intent(in) :: vn

   real(8),    dimension(nf,kspec),   intent(in) :: wt
   complex(8), dimension(npts,kspec), intent(in) :: yk

!  Output

   real(8), dimension(nf), intent(out)         :: nsspec, nsdiff, nsd2

!  Multitaper parameters

   complex(8), dimension(nf,kspec)   :: xk

!  Parameters

   integer, parameter   :: nal = 3
   integer              :: ic

   real(8)              :: w

!  Sinc2 matrix

   real(8), dimension(npts,npts) :: Ksinc

!  Al matrix and Covariance

   complex(8), dimension(kspec*kspec,nf)     :: C

!  Al eigenfunctions

   real(8), dimension(npts,nal)  :: A
   real(8), dimension(nal)       :: hl

!  QR Decomposition

   complex(8), dimension(nal)             :: btilde

!  The NS Quadratic model

   complex(8), dimension(nal)             :: qmodel
   complex(8), dimension(nf)              :: cte, slope, quad
   real(8),    dimension(nf)              :: cte_var, slope_var
   real(8),    dimension(nf)              :: quad_var, sigma2

!  Other parameters

   integer :: m, n, i, l, j, k
   real(8), parameter :: pi=3.141592653589793d0
   real(8)            :: scl

!  Save values

   integer, save                              :: npts_3, kspec_3
   real(8), save                              :: tbp_3

!  Ql, and QR matrices to save

   real(8), dimension(:,:), allocatable, save     :: Qvec 
   real(8), dimension(:,:), allocatable, save     :: Q3
   real(8), dimension(:,:), allocatable, save     :: Q_T3
   real(8), dimension(:,:), allocatable, save     :: R3
   real(8), dimension(:,:), allocatable, save     :: cov3


!********************************************************************

   ic=kspec*kspec   !int(kspec*(real(kspec+1)/2.))

!
! Create the weigthed xk = dk*yk
!

   do k = 1,kspec
      xk(:,k) = wt(:,k)*yk(1:nf,k)
   enddo

   w   = tbp / dble(npts)

!
!  Calculate Sinc^2 matrix and the eigenvalues, eigenvectors
!

   if (kspec/=kspec_3 .or. npts_3/=npts .or. tbp_3/=tbp ) then

      print *, 'Doing the Sinc^2 eigenfunctions'
      if (allocated(Q3)) then
         deallocate(Qvec,Q3, Q_T3,cov3,R3)
      endif
      allocate(Qvec(ic,nal))
      allocate(Q3(ic,nal))
      allocate(Q_T3(nal,ic))
      allocate(cov3(nal,nal))
      allocate(R3(nal,nal))

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

      call qrfac8(m,nal,Qvec,Q3,R3)
      call qrcov(nal,nal,R3(1:nal,1:nal),cov3)

      Q_T3 = transpose(Q3)

      npts_3 = npts
      kspec_3 = kspec
      tbp_3 = tbp
   endif

!
!  Create the vectorized Cjk matrix and Ql matrix {Vj Vk*}
!

   L = kspec*kspec

   m = 0 
   do j = 1,kspec
      do k = 1,kspec

         m = m + 1
         C(m,:) = ( conjg(xk(:,j)) * (xk(:,k)) )

      enddo
   enddo

!---
! Begin Least squares
!---

!
!  The least squares solution for the alpha vector
!

   do j = 1, nf

!     The back transformation (for complex variable)

      btilde = matmul(Q_T3,C(:,j))

      qmodel = 0.d0

      do i = nal, 1, -1
         do k = i+1, nal
            qmodel(i) = qmodel(i) - R3(i,k)*qmodel(k)
         enddo
         qmodel(i) = (qmodel(i) + btilde(i))/R3(i,i)
      enddo


      cte(j)   = (qmodel(1))    
      slope(j) = (qmodel(2))    
      quad(j)  = (qmodel(3))

      sigma2(j) = sum(abs( C(:,j) - matmul(Qvec,real(qmodel)) )**2)/dble(L-nal)

      cte_var(j)   = sigma2(j)*cov3(1,1)
      slope_var(j) = sigma2(j)*cov3(2,2)
      quad_var(j)  = sigma2(j)*cov3(3,3)

   enddo

   nsspec = abs(cte)
   nsdiff = real(slope)
   nsd2   = real(quad)

end subroutine nsqi


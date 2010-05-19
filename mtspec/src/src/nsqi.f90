subroutine nsqi(npts,dt,x,tbp,kspec,nf,nal,freq,spec,A,al,hl)

!
!  Compute the non-stationary QI basis functions and
!  eigenvalues. Output returns the al eigencoeffients
!  which can be used to obtain the time-derivative
!  of the spectrum.
!  We follow here 
!  An overview of multiple-window and Quadratic-inverse 
!  Spectrum Estimation Methods.
!  David J. Thomson
!  1994
!
!  FEBRUARY 2006
!  	Added the computation for the truncated version
!	of the matrix Ksinc, that is using only K dpss
!	functions. Based on Azadeh Moghtaderi (Queen's U.)
!	matlab codes.
!
!  Last Modified
!	German Prieto
!	Feb 16 2006
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
!	nal		number of QI coefficients to get
!
!  OUTPUT
!
!	freq(nf)	frequency vector
!	spec(nf)	the spectrum estimate
!	A(npts,nal)	the eigenfunctions of sinc^2 kernel
!	al(nf,nal)	the eigencoefficients for each freq.
!       hl(nal)		the eigenvalues of the Theta matrix
!
!  calls mtspec, dpss, rsm_eig 
!

!********************************************************************

   use spectra 

   implicit none

!  Input

   integer, intent(in)	:: npts, kspec, nf, nal

   real(4), intent(in)  :: dt, tbp

   real(4), dimension(npts), intent(in) :: x

!  Output

   real(4), dimension(nf), intent(out)       :: freq, spec
   real(4), dimension(npts,nal), intent(out) :: A
   real(4), dimension(nf,nal), intent(out)   :: al
   real(4), dimension(nal), intent(out)      :: hl

!
   
   integer :: i,j, n, m

   real(8), parameter :: pi=3.141592653589793d0, tpi = 2.d0*pi

   real(8) :: w

!  Time series and spectrum

   complex(4), dimension(npts,kspec) ::  yk 

   real(4), dimension(nf,kspec) :: wt

!  DPSS sequences

!  Dpss 

   real(8), dimension(kspec)      :: lambda, theta
   real(8), dimension(npts,kspec) :: vn

!  Al eigenfunctions

   real(8), dimension(npts,nal) :: A8
   real(8), dimension(nal) :: hl8

!  Al matrix

   integer :: l, k
   real(4), dimension(nal,kspec,kspec) :: Theta_ljk
   
   real(8), dimension(npts,npts) :: Ksinc
   real(4), dimension(nal)   :: trace
   real(8) :: scl
   
!  al eigencoefficients

   complex(4), dimension(nf,kspec) :: xk

   complex(4), dimension(nf,nal) :: al_cmp

!  Test covariance

!   complex(4), dimension(kspec,kspec) :: Cjk
!   complex(4), dimension(kspec,kspec) :: Cmodel

!********************************************************************

!  Get the spectrum estimate

   call mtspec(npts,dt,x,tbp,kspec,nf,freq,spec,   &
                yk=yk, wt=wt )

!
!  Get the dpss
!

   call dpss(npts,dble(tbp),kspec,vn,lambda,theta)

   write(6,'(3x,a/(4f18.14))')'Prolate spheroidal eigenvalues:',  &
     			       (lambda(j),j=1,kspec)

   w  = dble(tbp)/dble(npts)
  
!  Create the sinc^2 matrix

!   do n = 1,npts
!      do m = 1,npts
!      
!         if (n==m) then
!	    Ksinc(n,m) = (2.d0*w)**2;
!	 else   
!            Ksinc(n,m) = ( sin(2.d0*pi*w*dble(n-m))/(pi*dble(n-m)) )**2
!         endif
!
!      enddo
!   enddo
!
!   Ksinc = dble(N) * Ksinc

!  Truncated version of matrix

   do n = 1,npts
      do m = 1,npts
         Ksinc(n,m) =  sum(lambda(:)*(vn(n,:) * vn(m,:)))
      enddo
   enddo
   Ksinc = dble(npts)*(Ksinc**2);


   call rsm_eig( npts, Ksinc, nal, hl8, A8)
 
   write(6,'(3x,a/(4f18.6))')' Operator sinc^2 eigenvalues:',  &
     			       (hl8(j),j=1,nal)

!    Normalize so (1/N) sum from 1 to npts A(n,k)**2 = 1

   do i = 1,nal
         scl    = sum( A8(:,i) * A8(:,i) )
         A8(:,i) = A8(:,i) * sqrt( dble(npts)/scl )
   enddo
   
   A = real(A8)

!  Put positive standard
  
   do i = 1,nal
      if (A((npts+1)/2 + 1,i) < 0. ) then
         A(:,i) = -A(:,i)
      endif
   enddo

!  Create the [Theta_l]jk matrices A^l_jk from Thomson 1990
!  Note that this matrix is real, symmetric.
!  Test trace orthogonality

   do l = 1,nal
      do j = 1,kspec
         do k = 1,kspec
	    Theta_ljk(l,j,k) = 0.0
            do n = 1,npts 
               Theta_ljk(l,j,k) = Theta_ljk(l,j,k)              	&
                                + real(sqrt(lambda(j)*lambda(k)))     	&
	                        * (  real(vn(n,j)*vn(n,k)) * A(n,l) )
            enddo
         enddo
      enddo
   enddo
 
! Trace of the matrices

   trace = 0.0
   do l = 1,nal
      do j = 1,kspec
         do k = 1,kspec
            trace(l) = trace(l) + Theta_ljk(l,j,k)*Theta_ljk(l,k,j)
         enddo
      enddo
   enddo

   write(6,'(3x,a/(4f18.6))')' Theta matrix eigenvalues:',  &
     			       (trace(j),j=1,nal)

   hl = real(hl8)

!
! Create the weigthed xk = dk*yk
!

   do k = 1,kspec
      xk(:,k) = wt(:,k)*yk(1:nf,k)
   enddo


!
!  Create the nsQI coefficients a_l
!

   do l = 1,nal
      do k = 1,nf

         al_cmp(k,l) = sum ( matmul( conjg(xk(k,:)),Theta_ljk(l,:,:) )  &
	                 * xk(k,:) ) * 1./hl(l)
	          
      enddo
   enddo

   al = real(al_cmp)

   open(12,file='est_al.dat')
   do i = 1,nf
      write(12,*) freq(i), al(i,:)
   enddo
   close(12)

      
!   Cmodel = 0.
!   do j = 1,kspec
!      do k = 1,kspec
!
!         Cjk(j,k) = xk(53,j) * conjg(xk(53,k))
!    
!         do l = 1,nal
!            Cmodel(j,k) = Cmodel(j,k) + (al_cmp(53,l) * Theta_ljk(l,j,k))
!         enddo
!      enddo
!   enddo

end subroutine nsqi

!--------------------------------------------------------------------








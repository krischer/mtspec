subroutine set_xint(ising,w,x)

!
!  Modified
!	German Prieto
!	November 2004
!
!   Sets up weights and sample points for Ierley quadrature,
!
!   Slightly changed from original code, to avoid using common
!   blocks. Also avoided using some go to statements, not needed.
!
!   Input:
!	ising=1 	- integrand is analytic in closed interval
!	ising=2		- integrand may have bounded singularities
!			  at end points
!   
!   Returns: 
!   	w(nomx,lomx+1)	- weights
!    	x(lomx+1)	- sample points
!
!   lomx=number of samples = 2**nomx
!

!********************************************************************

   implicit none

   integer, parameter :: nomx=8, lomx=256
   real(8), parameter :: one=1.0d0, half=0.5d0
   
   integer :: k, i, nx, index, n, iprior, ising

   real(8) :: ck, rk, t, si, pin, pi

   real(8), dimension(nomx,lomx+1) :: w
   real(8), dimension(lomx+1)      :: x

!********************************************************************

   iprior = 0

   if (iprior .eq. ising) then 
      return
   endif
   
   iprior=ising

   pi=4.0d0*atan(one)
   n=2
   do index=1, nomx
      n=2*n
      nx=n-2
      if (index.eq.1) then
         nx=4
      endif
      pin=pi/dble(n)
      do i=0, n/2
         t=dble(i)*pin
         si=0.d0
      
         do k=0, nx, 2
            ck=4.0d0
            if (k.eq.0) then
  	       ck=2.0d0
  	    endif
            rk=dble(k)
            si=si+ck*cos(dble(k)*t)/(one-rk*rk)
         enddo

         if (i.eq.0.or.i.eq.n/2) then
	    si=half *si
	 endif

         t=cos(t)

	 if (ising == 2) then
            t=half *pi*(one +t)
            si=si*half * sin(t)*pi
            t=cos(t)
            x(i+1)=half *(one +t)
            w(index, i+1)=half *si/dble(n)
	 elseif (ising == 1) then
	    x(i+1)=half *(one +t)
            w(index, i+1)=half *si/dble(n)
         endif
      enddo
   enddo

   return
   
end subroutine set_xint


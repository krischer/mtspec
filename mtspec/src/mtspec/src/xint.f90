double precision function xint(a, b, tol,rpar,ipar)

!
!  Modified
!	German Prieto
!	November 2004
!
!   Quadrature by Ierley's method of Chebychev sampling.
!
!   This is a slight variation of Gleen Ierly's code. What was
!   mainly done, was to avoid use of common blocks, defining all
!   variables and performing the numerical integration inside
!   (previously done by function pssevf.
!
!   Exponential convergence rate for analytic functions!  Much faster 
!   than Romberg; competitive with Gauss integration, without awkward 
!   weights.
! 
!   Integrates the function dpsw on (a, b) to absolute
!   accuracy tol > 0.
!
!   the function in time is given by rpar with ipar points
!  
!   I removed the optional printing routine part of the code, 
!   to make it easier to read. I also moved both nval, etol
!   as normal variables inside the routine.
!
!   nval = number of function calls made by routine
!   etol = approximate magnitude of the error of the result
! 
!   NB:  subroutine  set_xint  is called once before  xint  to
!     provide quadrature samples and weights.
!
!     I also altered the subroutine call, to get the weights
!     and not save them in a common block, but get them 
!     directly back.
!
!   lomx=number of samples = 2**nomx
!
!   calls set_xint
!

!********************************************************************

   implicit none

   integer, parameter :: nomx=8, lomx=256

   integer :: i, index, im, im2, n
   integer :: nval
   integer :: ipar   !, init

   real(8), dimension(ipar) :: rpar
   real(8) :: f1, f2, sq, etol, a, b, tol

   real(8), dimension(lomx+1) :: fv
!   real(8), dimension(nomx-1) :: est
   real(8), dimension(nomx)   :: est

!  set_xint subroutine variables

   real(8), dimension(nomx,lomx+1) :: w
   real(8), dimension(lomx+1)      :: x

!  function part

   real(8) :: om, ct, st, y

   real(8), parameter :: pi=3.141592653589793d0, tpi= 2.d0*pi

!********************************************************************


!  if (init.eq. 0) then 

   call set_xint(1,w,x)

!   endif

! 
!   Check tol
!

   if (tol .le. 0.0d0) then
      write(6, '(a)') 'In xint tol must be > 0'
      stop
   endif

   n=1
   im=2**(nomx+1)

   do index=1, nomx
      n=2*n
      im=im/2
      im2=im/2
      if (index.le.1) then
         do i=0, n
	    ! Bottom
	    y = a+(b-a)*x(im2*i+1)
	    om = tpi*y
	    call sft(rpar,ipar,om,ct,st)
	    f1 = ct*ct+st*st
	    ! Top
	    y = b-(b-a)*x(im2*i+1)
	    om = tpi*y
	    call sft(rpar,ipar,om,ct,st)
	    f2 = ct*ct+st*st
         
            fv(im2*i+1)= f1 + f2   
         enddo
      else
          do i=1, n-1, 2
	     ! Bottom
	     y = a+(b-a)*x(im2*i+1)
	     om = tpi*y
	     call sft(rpar,ipar,om,ct,st)
	     f1 = ct*ct+st*st
	     ! Top
	     y = b-(b-a)*x(im2*i+1)
	     om = tpi*y
	     call sft(rpar,ipar,om,ct,st)
	     f2 = ct*ct+st*st
 
             fv(im2*i+1)= f1 + f2
          enddo
      endif
      
      xint=0.0d0
      do i=0, n
         xint=xint+w(index, i+1)*fv(im2*i+1)
      enddo
      xint=xint*(b-a)
      est(index)=xint
      etol=0.0d0
 
! 
!   Check for convergence. 
!

      nval=2*n
 
      if (index .eq. 2) then
         if (est(index) .eq. est(index-1)) then
            return
         endif

      elseif (index > 2) then
         sq=(est(index)-est(index-1))**2
         etol=sq/(0.01d0*sq + abs(est(index)-est(index-2)))
         if (etol .le. tol) then
            return
         endif
      endif
      
   enddo

   write(6,*)' xint unable to provide requested accuracy'
 
   return
      
end function xint



real function qtdis(p,df)

!
!  qtdis.f90  Quantiles of T (from S)
!
!  calls qsnorm
!

!********************************************************************

   real(4) :: fn,pr,a,b,c,d,x,y,pr2,fnp2,qsnorm
      
   integer n
      
   real(4), parameter :: pi=3.14159265e0
   real(4), parameter :: halfpi = pi/2.
   real(4), parameter :: zrotol = 1.e-12

!********************************************************************

   if((p.lt.0.).or.(p.gt.1.)) then
      write(6,'(a)') 'p not in (0,1)'
   endif

!   if(df.lt.1.) then
!      write(6,'(a)') 'dof < 1'
!   endif

   n = df
   fn = df
   if (p.lt.0.5) then
      pr = p*2.
   else
      pr = (1.-p)*2.
   endif

!  1 dof

   if (abs(fn-1.).le.zrotol) then
      pr = pr*halfpi
      qtdis = cos(pr)/sin(pr)
      if (p.lt.0.5) then
         qtdis = -qtdis
	 return
      endif
   endif

!                              2 dofs

   if (abs(fn-2.).le.zrotol) then
      qtdis = sqrt(2./(pr*(2.-pr))-2.)
      if (p.lt.0.5) then
         qtdis = -qtdis
	 return
      endif
   endif
   
   a = 1./(fn-0.5)
   b = 48./a**2
   c = ((20700.*a/b-98.)*a-16.)*a+96.36
   d = ((94.5/(b+c)-3.)/b+1.)*sqrt(a*halfpi)*fn
   x = d*pr
   y = x**(2./fn)

   if (y.le.(a+.05)) then
      fnp2 = fn+2.
      y = ((1./(((fn+6.)/(fn*y)-0.089*d-0.822)*fnp2*3.)    &
           +0.5/(fn+4.))*y-1.)*(fn+1.)/fnp2+1./y

   else 
!     asymptotic inverse expansion about normal

      pr2 = pr*0.5
      x = qsnorm(pr2)
      y = x**2
      if (fn.lt.5.) c = c+0.3*(fn-4.5)*(x+0.6)
      c = (((0.05*d*x-5.)*x-7.)*x-2.)*x+b+c
      y = (((((0.4*y+6.3)*y+36.)*y+94.5)/c-y-3.)/b+1.)*x
      y = a*y**2
      if (y.le.0.002) then
         y = 0.5*y**2+y
      else
         y = exp(y)-1.
      endif
   endif

   qtdis = sqrt(fn*y)

   if (p.lt.0.5) then
      qtdis = -qtdis
   endif

   return

end function qtdis


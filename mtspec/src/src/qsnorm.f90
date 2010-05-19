real function qsnorm(p)

!
!  qsnorm.f       Inverse standard normal
!
!              NBS 55 26.2.23 pg 933 ( from Hastings )
!
!  Modified
!	German Prieto
!	March 2005
!

!********************************************************************

   d=p
   
   if ((d.lt.0.).or.(d.gt.1.)) then
      write(*,*) "Argument ",p," in qsnorm not in [0,1]"
      write(6,'(a)') 'prob not in [0,1] in qsnorm'
   endif

   x=0.
   if (d-.5) 30,77,10

 10 d=1.-d
 30 t2=-2.*alog(d)
    t=sqrt(t2)
    x=t-(2.515517+.802853*t+.010328*t2)/(1.+1.432788*t+.189269*t2+   &
         .001308*t*t2)
    if(p.lt..5) x=-x
 77 qsnorm=x

   return
      
end function qsnorm

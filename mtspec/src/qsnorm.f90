real function qsnorm(p)

!$$$$$ calls no other routines
!  finds inverse cumulative normal probabilty function.  that is
!  if q(x)=p,  where  q  is the cumulative gaussian function,
!  finds  x  for the given  p.
!  notice  0 .lt. p .lt. 1.  if not, qsnorm is returned as 10**6,
!  which is so large it cannot be achieved with any computer-
!  representable data.
!

!********************************************************************

      implicit double precision (a-h, o-z)

!********************************************************************

      qsnorm=1e6
      if (p .le. 0.0 .or. p .ge. 1.0) return
!
      x=0.0
      d=p
      if (d .gt. 0.5) d=1.0 - p
      s=-2.0*log(d)
      t=sqrt(s)
      x=t - (2.515517 +t*(0.802853 +t*0.010328))/   &
            (1.0 + t*(1.432788 + t*(0.189269+ t*0.001308)))
      if (p .lt. 0.5) x=-x
      qsnorm=x
      return
      
end function qsnorm


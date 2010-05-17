function nearn(n)
!  calls nothing
!  Finds the largest integer less than or equal to n whose
!  prime factors are in the set {2, 3, 5}.
!
!  Code from Bob Parker (UCSD)
!
  
!********************************************************************

      implicit none

      integer :: n, nearn

      real(8) :: c , b, a

      real(8) :: p, dmin, d

      integer :: ka, kb, kc, ja, jb, jc

!********************************************************************

      c = log(dble(2))
      b = log(dble(3))
      a = log(dble(5))

      p = log(dble(n)+0.5d0)     ! Allow nearn=n to be searched

      ka=p/a
      kb=p/b
      kc=p/c

      dmin=p

      do 1500 ja=0, ka
         do 1400 jb=0, kb
             do 1300 jc=0, kc
               d = p - dble(ja)*a - dble(jb)*b - dble(jc)*c
               if (d .lt. 0.d0) goto 1400
               if (d.lt. dmin) dmin = d
 1300       continue
 1400    continue
 1500 continue

      nearn = nint(exp(p-dmin))

      return

end function nearn


!   imsl routine name   - mdfd
!
!-----------------------------------------------------------------------
!
!   computer            - cray/single
!
!   latest revision     - august 1, 1981
!
!   purpose             - f probability distribution function
!
!   usage               - call mdfd (f,n1,n2,p,ier)
!
!   arguments    f      - input constant to which integration is
!                           performed. f must be greater than or equal
!                           to zero.
!                n1     - input first degree of freedom. a positive
!                           integer.
!                n2     - input second degree of freedom, a positive
!                           integer.
!                p      - output probability that a random variable
!                           following the f distribution with degrees
!                           of freedom n1 and n2 will be less than or
!                           equal to input f.
!                ier    - error parameter. (output)
!                           terminal error
!                             ier = 129 indicates either n1 or n2 is
!                               less than one or n1+n2 is greater than
!                               20,000.  p is set to positive machine
!                               infinity.
!                             ier = 130 indicates f is less than zero.
!                               p is set to positive machine infinity.
!
!   precision/hardware  - single/all
!
!   reqd. imsl routines - merrc=erfc,uertst,ugetio
!
!   notation            - information on special notation and
!                           conventions is available in the manual
!                           introduction or through imsl routine uhelp
!
!
!   warranty            - imsl warrants only that imsl testing has been
!                           applied to this code. no other warranty,
!                           expressed or implied, is applicable.
!
!-----------------------------------------------------------------------
!
      subroutine fdis   (f,n1,n2,p,ier)
!
!                                  specifications for arguments

!********************************************************************

      integer, intent(in) :: n1,n2
      real, intent(in)    :: f
      
      integer, intent(out) :: ier
      real, intent(out) :: p

!     specifications for local variables

      integer            i1,i2p,i2,i,l2,mnm,mxm

      real acons,a,bige,b,cbr1,cbr2,c,dpl,dp,f1f,f1,f2p
      real                   f2,r1d3,r2d9,r2dpi,rinfp,sts,s,temp1,temp
      real                   theta,vp,x1,x2,xi

      data               rinfp/1.e37/
      data               r2d9/.22222222222222/,r1d3/.33333333333333/

!                                  r2dpi = 2/pi

      data               r2dpi/.63661977236758/
      data               bige/85.195648/,acons/1.e32/

!********************************************************************

!                                  test for invalid input
      mxm = max(n1,n2)
      mnm = min(n1,n2)
      if (mnm.lt.1.or.mxm.gt.(20000-mnm)) go to 100
      if (f.lt.0.0) go to 105
      ier = 0
      if (f.eq.0.0) go to 115
      f1 = n1
      f2 = n2
      dp = 0.0
      vp = f1+f2-2.0
      f1f = f1*f
      f2p = f2+f1f
      x1 = f2/f2p
      x2 = 1.0-x1
      if (x2.eq.0.0) go to 115
      if ((n1/2)*2-n1.eq.0.and.n1.le.500) go to 5
      if ((n2/2)*2-n2.eq.0.and.n2.le.500) go to 30
      if (n1+n2.le.500) go to 55
      f1 = r2d9/f1
      f2 = r2d9/f2
      cbr1 = r1d3*log(f)
      if (abs(cbr1).gt.bige) go to 120
      cbr1 = exp(cbr1)
      cbr2 = cbr1*cbr1
      s = (cbr1*(1.0-f2)-1.0+f1)/sqrt(f1+cbr2*f2)
      p=.70710678118655
      p = .5*erfc(-p*s)
      go to 95
!                                  n1 is even and less than 500
    5 temp1 = 0.
      temp = .5*f2*log(x1)
      if (n1.eq.2) go to 25
      i1 = n1-2
      xi = f1
      do 10 i2=2,i1,2
         l2 = i2
         xi = xi-2.
         vp = vp-2.
         dp = x2*vp/xi*(1.+dp)
         if (dp.gt.acons) go to 15
   10 continue
      go to 25
   15 if (l2.ge.i1) go to 25
      dpl = log(dp)
      i2p = l2+2
      xi = f1-i2p
      do 20 i2=i2p,i1,2
         vp = vp-2.
         dpl = dpl+log(x2*vp/xi)
         xi = xi-2.
   20 continue
      temp = temp+dpl
      if (abs(temp).le.bige) temp1 = exp(temp)
      p = 1.-temp1
      go to 95
   25 if (abs(temp).le.bige) temp1 = exp(temp)
      p = 1.0-temp1*(1.0+dp)
      go to 95
!                                  n2 is even and less than 500
   30 temp1 = 0.
      temp = .5*f1*log(x2)
      if (n2.eq.2) go to 50
      i1 = n2-2
      xi = f2
      do 35 i2=2,i1,2
         l2 = i2
         xi = xi-2.
         vp = vp-2.
         dp = x1*vp/xi*(1.+dp)
         if (dp.gt.acons) go to 40
   35 continue
      go to 50
   40 if (l2.ge.i1) go to 50
      dpl = log(dp)
      i2p = l2+2
      xi = f2-i2p
      do 45 i2=i2p,i1,2
         vp = vp-2.
         dpl = dpl+log(x1*vp/xi)
         xi = xi-2.
   45 continue
      temp = temp+dpl
      if (abs(temp).le.bige) temp1 = exp(temp)
      p = temp1
      go to 95
   50 if (abs(temp).le.bige) temp1 = exp(temp)
      p = temp1*(1.+dp)
      go to 95
!                                  sum of dfs are le 500 and odd
   55 dp = sqrt(f1f/f2)
      theta = atan(dp)
      sts = f1f/f2p
      a = 0.0
      b = 0.0
      if (n2.eq.1) go to 70
      if (n2.eq.3) go to 65
      i1 = n2-3
      xi = f2
      do 60 i2=2,i1,2
         xi = xi-2.
         a = x1*(xi-1.0)/xi*(1.0+a)
   60 continue
   65 a = x1*dp*(1.0+a)
   70 a = a+theta
      if (n1.eq.1) go to 90
      if (n1.eq.3) go to 80
      i1 = n1-3
      xi = f1
      do 75 i2=2,i1,2
         xi = xi-2.
         vp = vp-2.
         b = sts*vp/xi*(1.0+b)
   75 continue
   80 b = dp*x1*(1.0+b)
      if (n2.eq.1) go to 90
      i2 = n2/2
      c = 1.0
      do 85 i=1,i2
         b = b*x1*c/(c-0.5)
         c = c+1.0
   85 continue
   90 p = r2dpi*(a-b)
   95 if (p.lt.0.0) p = 0.0
      if (p.gt.1.0) p = 1.0
      go to 9005
  100 ier = 129
      go to 110
  105 ier = 130
  110 p = rinfp
      go to 9000
  115 p = 0.0
      go to 9005
  120 p = .5
      go to 9005
 9000 continue
!      call uertst (ier,6hmdfd  )
 9005 return

      end

! $Id: mdfd.f,v 1.1.1.1 1997/04/12 04:17:50 danq Exp $ 


!   imsl routine name   - merrc=erfc
!
!-----------------------------------------------------------------------
!
!   computer            - cray/single
!
!   latest revision     - august 1, 1981
!
!   purpose             - evaluate the complemented error function
!
!   usage               - result = erfc(y)
!
!   arguments    y      - input argument of the complemented error
!                           function.
!                erfc   - output value of the complemented error
!                           function.
!
!   precision/hardware  - single/all
!                         note - erfc may not be supplied by imsl if it
!                           resides in the mathematical subprogram
!                           library supplied by the manufacturer.
!
!   reqd. imsl routines - none required
!
!   notation            - information on special notation and
!                           conventions is available in the manual
!                           introduction or through imsl routine uhelp
!
!
!   warranty            - imsl warrants only that imsl testing has been
!                           applied to this code. no other warranty,
!                           expressed or implied, is applicable.
!
!-----------------------------------------------------------------------
!
      function erfc(y)
!                                  specifications for arguments
      real y
!                                  specifications for local variables
      integer            isw,i
      dimension          p(5),q(3),p1(8),q1(7),p2(5),q2(4)
      real p,q,p1,q1,p2,q2,xmin,xlarge,ssqpi,x
      real                  res,xsq,xnum,xden,xi

!                                  coefficients for 0.0 .le. y .lt.
!                                  .477
      data               p(1)/-.44422647396874/,  &
                        p(2)/10.731707253648/,    &
                        p(3)/15.915606197771/,    &
                        p(4)/374.81624081284/,    &
                        p(5)/2.5612422994823e-02/
      data               q(1)/17.903143558843/,   &
                        q(2)/124.82892031581/,    &
                        q(3)/332.17224470532/
!                                  coefficients for .477 .le. y
!                                  .le. 4.0
      data               p1(1)/7.2117582508831/,  &
                        p1(2)/43.162227222057/,   &
                        p1(3)/152.98928504694/,   &
                        p1(4)/339.32081673434/,   &
                        p1(5)/451.91895371187/,   &
                        p1(6)/300.45926102016/,   &
                        p1(7)/-1.3686485738272e-07/,   &
                        p1(8)/.56419551747897/
      data               q1(1)/77.000152935229/,  &
                        q1(2)/277.58544474399/,   &
                        q1(3)/638.98026446563/,   &
                        q1(4)/931.35409485061/,   &
                        q1(5)/790.95092532790/,   &
                        q1(6)/300.45926095698/,   &
                        q1(7)/12.782727319629/
!                                  coefficients for 4.0 .lt. y
      data               p2(1)/-.22695659353969/,      &
                        p2(2)/-4.9473091062325e-02/,   &
                        p2(3)/-2.9961070770354e-03/,   &
                        p2(4)/-2.2319245973418e-02/,   &
                        p2(5)/-2.7866130860965e-01/
      data               q2(1)/1.0516751070679/,      &
                        q2(2)/.19130892610783/,       &
                        q2(3)/1.0620923052847e-02/,   &
                        q2(4)/1.9873320181714/
!                                  constants
      data               xmin/1.0e-8/,xlarge/5.6875/
!                                  erfc(xbig) .approx. setap
      data               xbig/25.90625/
      data               ssqpi/.56418958354776/
!                                  first executable statement
      x = y
      isw = 1
      if (x.ge.0.0) go to 5
      isw = -1
      x = -x
    5 if (x.lt..477) go to 10
      if (x.le.4.0) go to 30
      if (isw .gt. 0) go to 40
      if (x.lt.xlarge) go to 45
      res = 2.0
      go to 65
!                                  abs(y) .lt. .477, evaluate
!                                  approximation for erfc
   10 if (x.lt.xmin) go to 20
      xsq = x*x
      xnum = p(5)
      do 15 i = 1,4
         xnum = xnum*xsq+p(i)
   15 continue
      xden = ((q(1)+xsq)*xsq+q(2))*xsq+q(3)
      res = x*xnum/xden
      go to 25
   20 res = x*p(4)/q(3)
   25 if (isw.eq.-1) res = -res
      res = 1.0e0-res
      go to 65
!                                  .477 .le. abs(y) .le. 4.0
!                                  evaluate approximation for erfc
   30 xsq = x*x
      xnum = p1(7)*x+p1(8)
      xden = x+q1(7)
      do 35 i=1,6
         xnum = xnum*x+p1(i)
         xden = xden*x+q1(i)
   35 continue
      res = xnum/xden
      go to 55
!                                  4.0 .lt. abs(y), evaluate
!                                  minimax approximation for erfc
   40 if (x.gt.xbig) go to 60
   45 xsq = x*x
      xi = 1.0e0/xsq
      xnum = p2(4)*xi+p2(5)
      xden = xi+q2(4)
      do 50 i = 1,3
         xnum = xnum*xi+p2(i)
         xden = xden*xi+q2(i)
   50 continue
      res = (ssqpi+xi*xnum/xden)/x
   55 res = res*exp(-xsq)
      if (isw.eq.-1) res = 2.0e0-res
      go to 65
   60 res = 0.0
   65 erfc = res
      return
      end

! $Id: erfc.f,v 1.1.1.1 1997/04/12 04:17:49 danq Exp $ 


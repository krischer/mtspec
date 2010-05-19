subroutine yule(iphase, nord,nx,x,nf,sx,ar)

!  calls toepl sft
!  Prewhitening of the spectrum.  See Percival and Walden,
!  "Spectral Analaysis ...", Chap 9, 1993.
!
!  nord = number of AR coeffs to be found.
!  iphase=1, uses spectral estimates  sx()  to calculate
!    autocovariances ar() for Yule-Walker equations.  Solves
!    Y-W equations then prewhitens time series x().
!  iphase=2, modifies sx() - undoes prewhitening by dividing
!    out the squared FT of the AR filter.
!
!  Original code from Robert L. Parker
!  Modified:
!	German A. Prieto
!	September 2007
!

!********************************************************************

      implicit none

      integer, intent(in)  :: iphase, nord, nx, nf
      real(4), dimension(nx), intent(in out)  :: x
      real(4), dimension(nf), intent(in out)  :: sx

      real(4), dimension(20), intent(in out) :: ar

      real(8), dimension(nf) :: sx2

      real(4), parameter :: pi=3.141592653589793
      integer, parameter :: ndmx=20

      integer                    :: nff
      real(8)                    :: ct, st
      real(4), dimension(2*ndmx) :: row, wrk
      real(4), dimension(ndmx)   :: rhs

      integer :: nrd, i, j, k, iok, jf, jx
      real(4) :: omi, sum, dom, ftsq


!********************************************************************

      if (nord.le. 1) return

      nrd=min(ndmx-1, nord)

      if (nrd .ne. nord) write(*,'(a,i3)')    &
         '>>> Order of prewhitening filter reduced to ',nrd

!  Phase 1: prewhiten time series.
!

   if (iphase==1) then

      sx2 = dble(sx)

      nff=nf-1

!  Fourier transform PSD estimate to find autocovariance function.
!  Load values into Toeplitz matrix and rhs vector

      row(nrd+2)=0.0
      do i=nrd+1, 1 , -1
         omi=pi*real(i-1)/real(nff)
         call sft(sx2,nff, dble(omi), ct, st)
         row(i) = real(ct)
         row(nrd+i)=row(i)
         rhs(i)=row(i+1)
         !print *, 'nff, Omega, row', nff, omi, row(i)
      enddo

!  Solve symmetric Toeplitz system for filter coeffs ar()

      ar(1)=-1.0
      call toepl(nrd, row, rhs, wrk, ar(2), iok)

!  Convolve filter ar() into series x(), overwriting

      do 1500 j=nx, 1, -1
        sum=x(j)
        do 1400 jf=2, nrd+1
          jx=max(1, j-jf+1)
          sum=sum - ar(jf)*x(jx)
 1400   continue
        x(j)=sum
 1500 continue

      !write(*,'(a/(1p,5g12.5))') 'Prewhitening AR filter:',   &
      !         (ar(j),j=1,nrd)

      return

!
!  Phase 2: Correct spectral estimates for prewhitening.
!  Uses ar() found in phase 1.

   elseif (iphase==2) then 
  
!  Divide out from sx() the AR spectrum of filter coeffs ar()
      do k=1, nf
         dom = pi*real(k-1) / real(nf)
         call sft(dble(ar), nrd+1, dble(dom), ct,st)
         ftsq= real(st)**2 + real(ct)**2
         sx(k)=sx(k) / ftsq
      enddo
 
   endif

   return
   
end subroutine yule

!--------------------------------------------------------------------

      subroutine toepl(n, r, y, ff, x, iok)

!  calls nothing
!  Solves linear system in Toeplitz form.
!  n    order of matrix.
!  r(1) ... r(n)     1st row of Toeplitz array.  Overwritten.
!  r(n+1) ... r(2*n) 1st col of Toeplitz array.  Overwritten.
!  y()    the rhs vector.
!  ff()   working array of size at least  2*n.
!  x()    the solution vector.
!  iok  1 if everything ok, 0 if singular principal minor.

!********************************************************************

      dimension r(*),x(*),y(*),ff(2,*)
!********************************************************************

!  Re-order  r  into crazy pattern:
!  r(1),r(2), ... r(n):       the first row of the matrix BACKWARDS!
!  r(n),r(n+1), ... r(2*n-1): the first column of the matrix.
!

      do 1100 i=1, n/2
        ri=r(i)
        r(i)=r(n-i+1)
        r(n-i+1)=ri
 1100 continue
      do 1150 i=n+1, 2*n
        r(i-1)=r(i)
 1150 continue

      iok=1
      if(r(n).eq.0.) goto 99
      x(1)=y(1)/r(n)
      if(n.eq.1)return
      ff(1,1)=r(n-1)/r(n)
      ff(2,1)=r(n+1)/r(n)
      do 15 m=1,n
        m1=m+1
        sxn=-y(m1)
        sd=-r(n)
        do 11 j=1,m
          sxn=sxn+r(n+m1-j)*x(j)
          sd=sd+r(n+m1-j)*ff(1,m-j+1)
11      continue
        if(sd.eq.0.)goto 99
        x(m1)=sxn/sd
        do 12 j=1,m
          x(j)=x(j)-x(m1)*ff(1,m-j+1)
12      continue
        if(m1.eq.n) return
        sgn=-r(n-m1)
        shn=-r(n+m1)
        sgd=-r(n)
        do 13 j=1,m
          sgn=sgn+r(n+j-m1)*ff(1,j)
          shn=shn+r(n+m1-j)*ff(2,j)
          sgd=sgd+r(n+j-m1)*ff(2,m-j+1)
13      continue
        if(sd.eq.0..or.sgd.eq.0.)goto 99
        ff(1,m1)=sgn/sgd
        ff(2,m1)=shn/sd
        k=m
        m2=(m+1)/2
        pp=ff(1,m1)
        qq=ff(2,m1)
        do 14 j=1,m2
          pt1=ff(1,j)
          pt2=ff(1,k)
          qt1=ff(2,j)
          qt2=ff(2,k)
          ff(1,j)=pt1-pp*qt2
          ff(1,k)=pt2-pp*qt1
          ff(2,j)=qt1-qq*pt2
          ff(2,k)=qt2-qq*pt1
          k=k-1
14      continue
15    continue
     
99    iok=0
      return

      end subroutine toepl



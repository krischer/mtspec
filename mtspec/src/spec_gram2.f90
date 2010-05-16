subroutine spec_gram2(npts,dt,x,tbp,kspec,nf,nwin,noverlap,ngram, &
                     freq,t,sgram,fdgram,tdgram,fd2gram,td2gram,      &
                     cte_var,fslope_var,fquad_var,     &
                     tslope_var,tquad_var, ntwin,Al)

!
!  Calculate the spectrogram of vector x using a nwin long running 
!  window (in samples) and overlap window noverlap (in samples). 
!

!********************************************************************

   use spectra

   implicit none

!  INPUT

   integer, intent(in)   :: npts, kspec, nf, nwin, noverlap, ngram, ntwin
   real(8), intent(in)   :: dt, tbp

   real(8), dimension(npts), intent(in) :: x

!  OUTPUT

   real(8), intent(out), dimension(nf)        :: freq
   real(8), intent(out), dimension(nf)        :: t
   real(8), intent(out), dimension(ngram,nf)  :: sgram
   real(8), intent(out), dimension(ngram,nf)  :: fdgram, tdgram
   real(8), intent(out), dimension(ngram,nf)  :: fd2gram, td2gram
   real(8), intent(out), dimension(ngram,nf)  :: cte_var
   real(8), intent(out), dimension(ngram,nf)  :: fslope_var, tslope_var
   real(8), intent(out), dimension(ngram,nf)  :: fquad_var, tquad_var
   real(4), intent(out), dimension(ntwin,3)   :: Al

!  Spectrum for small window

   real(8), dimension(nf)        :: spec
   real(8), dimension(nwin)      :: x2

!  The time vector

   real(8), dimension(npts)      :: tlong

!  Input

   real(8)              :: w

!  The NS Quadratic model

   real(8),    dimension(nf)              :: cte, slope, quad, qiquad,qislope
   real(8),    dimension(nf)              :: specvar, ctevar, qivar, nsvar
   real(8),    dimension(nf)              :: qi2var, ns2var

!  Other parameters

   integer :: m, n, i, l, j, k, n1, njump, icnt, ngram2

!  For now, create vector and do multitaper

   real(8)                           :: df
   real(8),    dimension(nf,kspec)   :: wt
   complex(8), dimension(nwin,kspec) :: yk
   complex(8), dimension(nf,kspec)   :: xk

! The time orthogonal functions

   real(8), dimension(nwin,3)  :: A

! Filename

   character (len=100) :: file1, file2, outfile

! Save values

   integer, save                              :: nwin_2, kspec_2
   real(8), save                              :: tbp_2

   real(8), dimension(:),   allocatable, save :: lambda, theta
   real(8), dimension(:,:), allocatable, save :: vn

!********************************************************************

!  Test all the integer values, so that it actually corresponds to
!  what is expected. 

   njump = nwin - noverlap
   if (njump <= 0) then
      write(6,'(a,i5)') 'ERROR, noverlap is larger than nwin; STOPPED ' 
      stop
   endif

   print *, nwin, noverlap, njump

   ngram2 = (npts - nwin - 1)/njump +1

   if (ngram /= ngram2) then
      write(6,'(a)') 'ERROR, ngram is inconsistent; STOPPED ' 
      write(6,'(a)') 'ngram = (npts - nwin - 1)/njump + 1'
      stop
   endif

!  Generate time vector

   tlong = dble( (/(i-1, i=1,npts)/) )*dt

!  The time bandwidth product
   
   w   = tbp / dble(nwin)

!
!  Get the dpss (if already saved, don't compute)
!


   if (kspec/=kspec_2 .or. nwin_2/=nwin .or. tbp_2/=tbp .or. &
       .not. allocated(vn) ) then
     
      if (allocated(vn)) then
         deallocate(vn, lambda, theta)
      endif
      allocate(vn(nwin,kspec))
      allocate(lambda(kspec),theta(kspec))

      if (nwin<20000) then
   
         call dpss(nwin,tbp,kspec,vn,lambda,theta)

      else

         call dpss_spline(10000,nwin,tbp,kspec,vn,lambda,theta)
   
      endif
      nwin_2 = nwin
      kspec_2 = kspec
      tbp_2 = tbp
   endif

!  Do multitaper

   icnt = 0
   do i = 1,npts,njump

      if (i > npts-nwin) then
         exit
      endif

      if (mod(i,1000)==0) then
         print *, i
      endif

      j = i+nwin/2

      icnt = icnt + 1

      x2(1:nwin) = x(i:i+nwin-1)
      x2 = x2 - sum(x2)/dble(nwin)

      call mtspec(nwin,dt,x2,tbp,kspec,nf,freq,spec,yk=yk,wt=wt)

      df = freq(2)

      call qi_nsqi3(nwin,tbp,kspec,nf,lambda,vn,yk,wt,  &
                    spec,qislope,qiquad,slope,quad,       &
                    specvar,qivar,qi2var,               &
                    ctevar,nsvar,ns2var,A) 


!     Normalize so that df = 1
!      qislope = qislope*df

      sgram(icnt,:)   = spec
      fdgram(icnt,:)  = qislope
      fd2gram(icnt,:) = qiquad

      tdgram(icnt,:)  = slope
      td2gram(icnt,:) = quad

!  Put variance estiamtes in matrix

      cte_var(icnt,:)    = specvar
      fslope_var(icnt,:) = qivar
      tslope_var(icnt,:) = nsvar
      fquad_var(icnt,:)  = qi2var
      tquad_var(icnt,:)  = ns2var

!     The time vector as output

      t(icnt) = tlong(j)

   enddo

!  Integrate tslope_gram over time
!

!  POR QUE ESTOY HACIENDO ESTO?
!   tdgram     = tdgram*w !*2.d0
!   tslope_var = tslope_var*(w**2) !*4.d0


   do i = 1,ntwin
      Al(i,:) = real(A((i-1)*njump+1,:))
   enddo

   return


end subroutine spec_gram2


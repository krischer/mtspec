subroutine spec_gram(npts,dt,x,tbp,kspec,nf,nwin,noverlap,ngram, &
                     freq,t,sgram,fdgram,tdgram,                 &
                     cte_var,fslope_var,cte_var2,tslope_var)

!
!  Calculate the spectrogram of vector x using a nwin long running 
!  window (in samples) and overlap window noverlap (in samples). 
!

!********************************************************************

   use spectra

   implicit none

!  INPUT

   integer, intent(in)   :: npts, kspec, nf, nwin, noverlap, ngram
   real(8), intent(in)   :: dt, tbp

   real(8), dimension(npts), intent(in) :: x

!  OUTPUT

   real(8), intent(out), dimension(nf)        :: freq
   real(8), intent(out), dimension(nf)        :: t
   real(8), intent(out), dimension(ngram,nf)  :: sgram
   real(8), intent(out), dimension(ngram,nf)  :: fdgram, tdgram
   real(8), intent(out), dimension(ngram,nf)  :: cte_var, cte_var2
   real(8), intent(out), dimension(ngram,nf)  :: fslope_var, tslope_var

!  Spectrum for small window

   real(8), dimension(nf)        :: spec

!  The time vector

   real(8), dimension(npts)      :: tlong

!  Input

   real(8)              :: w

!  The NS Quadratic model

   real(8),    dimension(nf)              :: cte, slope, quad, qislope
   real(8),    dimension(nf)              :: specvar, ctevar, qivar, nsvar

!  Other parameters

   integer :: m, n, i, l, j, k, n1, njump, icnt, ngram2

!  For now, create vector and do multitaper

   real(8)                           :: df
   real(8),    dimension(nf,kspec)   :: wt
   complex(8), dimension(nwin,kspec) :: yk
   complex(8), dimension(nf,kspec)   :: xk

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

      call mtspec(nwin,dt,x(i:i+nwin),tbp,kspec,nf,freq,spec,yk=yk,wt=wt)

      df = freq(2)

      call qi_nsqi(nwin,tbp,kspec,nf,lambda,vn,yk,wt,  &
                    spec,qislope,cte,slope,quad,       &
                    specvar,qivar,ctevar,nsvar) 

!     Normalize so that df = 1
!      qislope = qislope*df

      tdgram(icnt,:) = slope
      fdgram(icnt,:) = qislope

      sgram(icnt,:) = spec

!  Put variance estiamtes in matrix

      cte_var(icnt,:)    = specvar
      cte_var2(icnt,:)   = ctevar
      fslope_var(icnt,:) = qivar
      tslope_var(icnt,:) = nsvar

!     The time vector as output

      t(icnt) = tlong(j)

   enddo

!  Integrate tslope_gram over time
!

   tdgram     = tdgram*w !*2.d0
   tslope_var = tslope_var*(w**2) !*4.d0

   return


end subroutine spec_gram


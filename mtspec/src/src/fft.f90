subroutine fft(data,npts)

!
!  Modified
!	German Prieto
!	November 2004
!
!  The discrete fourier tranform of a complex data set of size npts
!
!  Discussion:
!
! 	Note here that we are working with double precision, to keep 
! 	the notation from the multitaper code accordingly. 
!	This FFT does not seem to have the limitations of other 
!	codes, in the sense that it is capable of computing FFTs
!      	for any size of the data. 
!
!  Input/Output 	
!
!	data	the complex data to be transformed, data(npts)
!
!  Input
!
!	npts	the length of the series data
!
!  calls cffti, cfftf
!

!**********************************************************************

   implicit none

   integer, intent(in) :: npts
   
   real(8), dimension(4*npts+15) :: wsave

   complex(8), intent(in out), dimension(npts) :: data
   
!**********************************************************************

   call cffti(npts, wsave)

   call cfftf(npts, data, wsave)

   return

end subroutine fft

!======================================================================

subroutine cfftf ( n, c, wsave )

!
!
!  CFFTF computes the forward complex discrete Fourier transform.
!
!
!  Discussion:
!
!    This process is sometimes called Fourier analysis.
!
!    CFFTF computes the Fourier coefficients of a complex periodic sequence.
!
!    The transform is not normalized.  To obtain a normalized transform,
!    the output must be divided by N.  Otherwise a call of CFFTF
!    followed by a call of CFFTB will multiply the sequence by N.
!
!    The array WSAVE must be initialized by calling CFFTI.
!
!    The transform is defined by:
!
!      C_out(J) = sum ( 1 <= K <= N ) 
!        C_in(K) * exp ( - sqrt ( -1 ) * ( J - 1 ) * ( K - 1 ) * 2 * PI / N )
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Clever Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    P N Swarztrauber, 
!    Vectorizing the FFT's, 
!    in Parallel Computations (G. Rodrigue, editor), 
!    Academic Press, 1982, pages 51-83.
!
!    B L Buzbee, 
!    The SLATEC Common Math Library, 
!    in Sources and Development of Mathematical Software (W. Cowell, editor),
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer N, the length of the sequence to be transformed.  
!    The method is more efficient when N is the product of small primes.
!
!    Input/output, complex C(N).
!    On input, the data sequence to be transformed.
!    On output, the Fourier coefficients.
!
!    Input, real WSAVE(4*N+15).  The array must be initialized by calling 
!    CFFTI.  A different WSAVE array must be used for each different
!    value of N. 
!

!**********************************************************************

  implicit none

  integer n

  complex(8), dimension(n) :: c

  real(8) wsave(4*n+15)

!**********************************************************************

  if ( n <= 1 ) then
    return
  end if

  call cfftf1 ( n, c, wsave(1), wsave(2*n+1), wsave(4*n+1) )

  return
end

!======================================================================

subroutine cfftf1 ( n, c, ch, wa, ifac )

!
!
! CFFTF1 is a lower level routine used by CFFTF.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the length of the sequence to be transformed.  
!
!    Input/output, complex C(N).
!    On input, the data sequence to be transformed.
!    On output, the Fourier coefficients.
!
!    Input, complex CH(N).
!
!    Input, real WA(2*N).
!
!    Input, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!

!**********************************************************************

   implicit none
   
   integer :: n, idl1, ido, ip, iw, ix2, ix3, ix4
   integer :: k1, l1, l2, na, nac, nf

   integer, dimension(15) :: ifac

   real(8), dimension(2*n) :: wa 

   complex(8), dimension(n) :: c, ch

!**********************************************************************

  nf = ifac(2)
  na = 0
  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    l2 = ip * l1
    ido = n / l2
    idl1 = 2 * ido * l1

    if ( ip == 4 ) then

      ix2 = iw + 2 * ido
      ix3 = ix2 + 2 * ido
 
      if ( na == 0 ) then
        call passf4 ( 2*ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call passf4 ( 2*ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

      na = 1 - na

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call passf2 ( 2*ido, l1, c, ch, wa(iw) )
      else
        call passf2 ( 2*ido, l1, ch, c, wa(iw) )
      end if

      na = 1 - na

    else if ( ip == 3 ) then

      ix2 = iw + 2 * ido

      if ( na == 0 ) then
        call passf3 ( 2*ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call passf3 ( 2*ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

      na = 1 - na

    else if ( ip == 5 ) then

      ix2 = iw + 2 * ido
      ix3 = ix2 + 2 * ido
      ix4 = ix3 + 2 * ido

      if ( na == 0 ) then
        call passf5 ( 2*ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call passf5 ( 2*ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

      na = 1 - na

    else

      if ( na == 0 ) then
        call passf ( nac, 2*ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
      else
        call passf ( nac, 2*ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
      end if

      if ( nac /= 0 ) then
        na = 1 - na
      end if

    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * 2 * ido

  end do

  if ( na /= 0 ) then
    c(1:n) = ch(1:n)
  end if

  return
end

!======================================================================

subroutine cffti ( n, wsave )

!
!
! CFFTI initializes WSAVE, used in CFFTF and CFFTB. 
!
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the 
!    trigonometric functions are computed and stored in WSAVE.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Clever Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    P N Swarztrauber, 
!    Vectorizing the FFT's, 
!    in Parallel Computations (G. Rodrigue, editor), 
!    Academic Press, 1982, pages 51-83.
!
!    B L Buzbee, 
!    The SLATEC Common Math Library, 
!    in Sources and Development of Mathematical Software (W. Cowell, editor),
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer N, the length of the sequence to be transformed.
!
!    Output, real WSAVE(4*N+15), contains data, dependent on the value
!    of N, which is necessary for the CFFTF or CFFTB routines.  
!

!**********************************************************************

   implicit none

   integer n

   real(8) :: wsave(4*n+15)
   
!**********************************************************************

  if ( n <= 1 ) then
    return
  end if

  call cffti1 ( n, wsave(2*n+1), wsave(4*n+1) )

  return
end

!======================================================================

subroutine cffti1 ( n, wa, ifac )

!
!
! CFFTI1 is a lower level routine used by CFFTI.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the length of the sequence to be transformed.
!
!    Input, real WA(2*N).
!
!    Input, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!

!**********************************************************************

   implicit none

   integer :: n, i, i1, ido, ii, ip, j, k1, l1, l2, ld, nf

   integer, dimension(15) :: ifac
   
   real(8) :: arg, argh, argld, fi

   real(8), dimension(2*n) :: wa

   real(8), parameter :: pi=3.141592653589793238d0

!**********************************************************************

  call i_factor ( n, ifac )

  nf = ifac(2)

  argh = 2.0d0 * pi / dble( n )
  
  i = 2
  l1 = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    ld = 0
    l2 = l1 * ip
    ido = n / l2

    do j = 1, ip-1

      i1 = i
      wa(i-1) = 1.0d0
      wa(i) = 0.0d0
      ld = ld + l1
      fi = 0.0d0
      argld = dble ( ld ) * argh

      do ii = 4, 2*ido+2, 2
        i = i + 2
        fi = fi + 1.0d0
        arg = fi * argld
        wa(i-1) = cos ( arg )
        wa(i) = sin ( arg )
      end do

      if ( ip > 5 ) then
        wa(i1-1) = wa(i-1)
        wa(i1) = wa(i)
      end if

    end do

    l1 = l2

  end do

  return
end

!======================================================================

subroutine i_factor ( n, ifac )

!
!
! I_FACTOR factors an integer.
!
!
!  Modified:
!
!    14 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the number to be factored.
!
!    Output, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!

!**********************************************************************

   implicit none

   integer i, j, n
   integer nf, nl, nq, nr, ntry

   integer, dimension(15) :: ifac

!**********************************************************************

  ifac(1) = n

  nf = 0
  nl = n

  if ( n == 0 ) then
    nf = 1
    ifac(2) = nf
    ifac(2+nf) = 0
    return
  end if

  if ( n < 1 ) then
    nf = nf + 1
    ifac(2+nf) = -1
    nl = - n
  end if

  if ( nl == 1 ) then
    nf = nf + 1
    ifac(2) = nf
    ifac(2+nf) = 1
    return
  end if

  j = 0

  do while ( nl > 1 )

    j = j + 1
!
!  Choose a trial divisor, NTRY.
!
    if ( j == 1 ) then
      ntry = 4
    else if ( j == 2 ) then
      ntry = 2
    else if ( j == 3 ) then
      ntry = 3
    else if ( j == 4 ) then
      ntry = 5
    else
      ntry = ntry + 2
    end if
!
!  Divide by the divisor as many times as possible.
!
    do

      nq = nl / ntry
      nr = nl - ntry * nq

      if ( nr /= 0 ) then
        exit
      end if

      nl = nq
      nf = nf + 1
!
!  Make sure factors of 2 appear in the front of the list.
!
      if ( ntry /= 2 ) then

        ifac(2+nf) = ntry

      else

        do i = nf, 2, -1
          ifac(i+2) = ifac(i+1)
        end do
        ifac(3) = 2

      end if

    end do

  end do

  ifac(2) = nf

  return
end

!======================================================================

subroutine passf ( nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )

!
!
! PASSF is a lower level routine used by CFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!

!**********************************************************************

   implicit none

   integer :: idl1, ido, ip, l1, i, idij
   integer :: idj, idl, idlj, idp, ik, inc
   integer :: ipph, j, jc, k, l, lc, nac, nt

   real(8) :: wai, war

   real(8), dimension(*) :: wa
   real(8), dimension(ido,l1,ip) :: c1
   real(8), dimension(idl1,ip)   :: c2
   real(8), dimension(ido,ip,l1) :: cc
   real(8), dimension(ido,l1,ip) :: ch
   real(8), dimension(idl1,ip)   :: ch2

!**********************************************************************

  nt = ip * idl1
  ipph = (ip+1) / 2
  idp = ip * ido

  if ( ido >= l1 ) then

    do j = 2, ipph
      jc = ip + 2 - j
      ch(1:ido,1:l1,j)  = cc(1:ido,j,1:l1) + cc(1:ido,jc,1:l1)
      ch(1:ido,1:l1,jc) = cc(1:ido,j,1:l1) - cc(1:ido,jc,1:l1)
    end do

    ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  else

    do j = 2, ipph
      jc = ip + 2 - j
      ch(1:ido,1:l1,j)  = cc(1:ido,j,1:l1) + cc(1:ido,jc,1:l1)
      ch(1:ido,1:l1,jc) = cc(1:ido,j,1:l1) - cc(1:ido,jc,1:l1)
    end do

    ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  end if

  idl = 2 - ido
  inc = 0

  do l = 2, ipph

    lc = ip + 2 - l
    idl = idl + ido

    do ik = 1, idl1
      c2(ik,l)  = ch2(ik,1) + wa(idl-1) * ch2(ik,2)
      c2(ik,lc) =           - wa(idl)   * ch2(ik,ip)
    end do

    idlj = idl
    inc = inc + ido

    do j = 3, ipph

      jc = ip + 2 - j

      idlj = idlj + inc
      if ( idlj > idp ) then
        idlj = idlj - idp
      end if

      war = wa(idlj-1)
      wai = wa(idlj)

      do ik = 1, idl1
        c2(ik,l)  = c2(ik,l)  + war * ch2(ik,j)
        c2(ik,lc) = c2(ik,lc) - wai * ch2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + ch2(1:idl1,j)
  end do

  do j = 2, ipph
    jc = ip + 2 - j
    do ik = 2, idl1, 2
      ch2(ik-1,j)  = c2(ik-1,j) - c2(ik,jc)
      ch2(ik-1,jc) = c2(ik-1,j) + c2(ik,jc)
      ch2(ik,j)    = c2(ik,j)   + c2(ik-1,jc)
      ch2(ik,jc)   = c2(ik,j)   - c2(ik-1,jc)
    end do
  end do

  if ( ido == 2 ) then
    nac = 1
    return
  end if

  nac = 0

  c2(1:idl1,1)    = ch2(1:idl1,1)
  c1(1,1:l1,2:ip) = ch(1,1:l1,2:ip)
  c1(2,1:l1,2:ip) = ch(2,1:l1,2:ip)

  if ( ( ido / 2 ) <= l1 ) then

    idij = 0
    do j = 2, ip
      idij = idij + 2
      do i = 4, ido, 2
        idij = idij + 2
        c1(i-1,1:l1,j) = wa(idij-1) * ch(i-1,1:l1,j) + wa(idij) * ch(i,1:l1,j)
        c1(i,1:l1,j)   = wa(idij-1) * ch(i,1:l1,j)   - wa(idij) * ch(i-1,1:l1,j)
      end do
    end do

  else

    idj = 2 - ido

    do j = 2, ip
      idj = idj + ido
      do k = 1, l1
        idij = idj
        do i = 4, ido, 2
          idij = idij + 2
          c1(i-1,k,j) = wa(idij-1) * ch(i-1,k,j) + wa(idij) * ch(i,k,j)
          c1(i,k,j)   = wa(idij-1) * ch(i,k,j)   - wa(idij) * ch(i-1,k,j)
        end do
      end do
    end do

  end if

  return
end

!======================================================================

subroutine passf2 ( ido, l1, cc, ch, wa1 )

!
!
! PASSF2 is a lower level routine used by CFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!

!**********************************************************************

   implicit none
 
   integer :: ido, l1, i, k

   real(8) :: ti2
   real(8) :: tr2
  
   real(8), dimension(ido,2,l1) :: cc
   real(8), dimension(ido,l1,2) :: ch
   real(8), dimension(ido)      :: wa1

!**********************************************************************

  if ( ido <= 2 ) then

    ch(1,1:l1,1) = cc(1,1,1:l1) + cc(1,2,1:l1)
    ch(1,1:l1,2) = cc(1,1,1:l1) - cc(1,2,1:l1)
    ch(2,1:l1,1) = cc(2,1,1:l1) + cc(2,2,1:l1)
    ch(2,1:l1,2) = cc(2,1,1:l1) - cc(2,2,1:l1)

  else

    do k = 1, l1
      do i = 2, ido, 2

        ch(i-1,k,1) = cc(i-1,1,k) + cc(i-1,2,k)
        tr2         = cc(i-1,1,k) - cc(i-1,2,k)

        ch(i,k,1) = cc(i,1,k) + cc(i,2,k)
        ti2       = cc(i,1,k) - cc(i,2,k)

        ch(i,k,2)   = wa1(i-1) * ti2 - wa1(i) * tr2
        ch(i-1,k,2) = wa1(i-1) * tr2 + wa1(i) * ti2

      end do
    end do

  end if

  return
end

!======================================================================

subroutine passf3 ( ido, l1, cc, ch, wa1, wa2 )

!
!
! PASSF3 is a lower level routine used by CFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!

!**********************************************************************

   implicit none

   integer :: ido, l1, i, k

   real(8) :: ci2, ci3, cr2, cr3, di2
   real(8) :: di3, dr2, dr3, ti2, tr2

   real(8), dimension(ido,3,l1) :: cc
   real(8), dimension(ido,l1,3) :: ch
   real(8), dimension(ido)      :: wa1, wa2

   real(8), parameter :: taui = -0.866025403784439d0
   real(8), parameter :: taur = -0.5d0

!**********************************************************************

  if ( ido == 2 ) then

    do k = 1, l1

      tr2 = cc(1,2,k) + cc(1,3,k)
      cr2 = cc(1,1,k) + taur * tr2
      ch(1,k,1) = cc(1,1,k) + tr2

      ti2 = cc(2,2,k) + cc(2,3,k)
      ci2 = cc(2,1,k) + taur * ti2
      ch(2,k,1) = cc(2,1,k) + ti2

      cr3 = taui * ( cc(1,2,k) - cc(1,3,k) )
      ci3 = taui * ( cc(2,2,k) - cc(2,3,k) )

      ch(1,k,2) = cr2 - ci3
      ch(1,k,3) = cr2 + ci3
      ch(2,k,2) = ci2 + cr3
      ch(2,k,3) = ci2 - cr3

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        tr2 = cc(i-1,2,k) + cc(i-1,3,k)
        cr2 = cc(i-1,1,k) + taur * tr2
        ch(i-1,k,1) = cc(i-1,1,k) + tr2

        ti2 = cc(i,2,k) + cc(i,3,k)
        ci2 = cc(i,1,k) + taur * ti2
        ch(i,k,1) = cc(i,1,k) + ti2

        cr3 = taui * ( cc(i-1,2,k) - cc(i-1,3,k) )
        ci3 = taui * ( cc(i,2,k)   - cc(i,3,k) )

        dr2 = cr2 - ci3
        dr3 = cr2 + ci3
        di2 = ci2 + cr3
        di3 = ci2 - cr3

        ch(i,k,2)   = wa1(i-1) * di2 - wa1(i) * dr2
        ch(i-1,k,2) = wa1(i-1) * dr2 + wa1(i) * di2
        ch(i,k,3)   = wa2(i-1) * di3 - wa2(i) * dr3
        ch(i-1,k,3) = wa2(i-1) * dr3 + wa2(i) * di3

      end do
    end do

  end if

  return
end

!======================================================================

subroutine passf4 ( ido, l1, cc, ch, wa1, wa2, wa3 )

!
!
! PASSF4 is a lower level routine used by CFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!

!**********************************************************************

   implicit none
  
   integer ido, l1, i, k

   real(8) :: ci2, ci3, ci4, cr2, cr3
   real(8) :: cr4, ti1, ti2, ti3, ti4
   real(8) :: tr1, tr2, tr3, tr4

   real(8), dimension(ido,4,l1) :: cc
   real(8), dimension(ido,l1,4) :: ch
   real(8), dimension(ido) :: wa1, wa2, wa3

!**********************************************************************

  if ( ido == 2 ) then

    do k = 1, l1

      ti1 = cc(2,1,k) - cc(2,3,k)
      ti2 = cc(2,1,k) + cc(2,3,k)
      tr4 = cc(2,2,k) - cc(2,4,k)
      ti3 = cc(2,2,k) + cc(2,4,k)
      tr1 = cc(1,1,k) - cc(1,3,k)
      tr2 = cc(1,1,k) + cc(1,3,k)
      ti4 = cc(1,4,k) - cc(1,2,k)
      tr3 = cc(1,2,k) + cc(1,4,k)

      ch(1,k,1) = tr2 + tr3
      ch(1,k,3) = tr2 - tr3
      ch(2,k,1) = ti2 + ti3
      ch(2,k,3) = ti2 - ti3
      ch(1,k,2) = tr1 + tr4
      ch(1,k,4) = tr1 - tr4
      ch(2,k,2) = ti1 + ti4
      ch(2,k,4) = ti1 - ti4

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        ti1 = cc(i,1,k)   - cc(i,3,k)
        ti2 = cc(i,1,k)   + cc(i,3,k)
        ti3 = cc(i,2,k)   + cc(i,4,k)
        tr4 = cc(i,2,k)   - cc(i,4,k)
        tr1 = cc(i-1,1,k) - cc(i-1,3,k)
        tr2 = cc(i-1,1,k) + cc(i-1,3,k)
        ti4 = cc(i-1,4,k) - cc(i-1,2,k)
        tr3 = cc(i-1,2,k) + cc(i-1,4,k)

        ch(i-1,k,1) = tr2 + tr3
        cr3         = tr2 - tr3
        ch(i,k,1)   = ti2 + ti3
        ci3         = ti2 - ti3

        cr2 = tr1 + tr4
        cr4 = tr1 - tr4
        ci2 = ti1 + ti4
        ci4 = ti1 - ti4

        ch(i-1,k,2) = wa1(i-1) * cr2 + wa1(i) * ci2
        ch(i,k,2)   = wa1(i-1) * ci2 - wa1(i) * cr2
        ch(i-1,k,3) = wa2(i-1) * cr3 + wa2(i) * ci3
        ch(i,k,3)   = wa2(i-1) * ci3 - wa2(i) * cr3
        ch(i-1,k,4) = wa3(i-1) * cr4 + wa3(i) * ci4
        ch(i,k,4)   = wa3(i-1) * ci4 - wa3(i) * cr4

      end do
    end do

  end if

  return
end


!======================================================================

subroutine passf5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )

!
!
! PASSF5 is a lower level routine used by CFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!

!**********************************************************************

   implicit none

   integer ido, l1, i, k

   real(8) :: ci2, ci3, ci4, ci5, cr2, cr3
   real(8) :: cr4, cr5, di2, di3, di4, di5
   real(8) :: dr2, dr3, dr4, dr5, ti2, ti3
   real(8) :: ti4, ti5, tr2, tr3, tr4, tr5

   real(8), dimension(ido,5,l1) :: cc
   real(8), dimension(ido,l1,5) :: ch
   real(8), dimension(ido)      :: wa1, wa2, wa3, wa4

   real(8), parameter :: ti11 = -0.951056516295154d0
   real(8), parameter :: ti12 = -0.587785252292473d0
   real(8), parameter :: tr11 =  0.309016994374947d0
   real(8), parameter :: tr12 = -0.809016994374947d0

!**********************************************************************

  if ( ido == 2 ) then

    do k = 1, l1

      ti5 = cc(2,2,k) - cc(2,5,k)
      ti2 = cc(2,2,k) + cc(2,5,k)
      ti4 = cc(2,3,k) - cc(2,4,k)
      ti3 = cc(2,3,k) + cc(2,4,k)
      tr5 = cc(1,2,k) - cc(1,5,k)
      tr2 = cc(1,2,k) + cc(1,5,k)
      tr4 = cc(1,3,k) - cc(1,4,k)
      tr3 = cc(1,3,k) + cc(1,4,k)

      ch(1,k,1) = cc(1,1,k) + tr2 + tr3
      ch(2,k,1) = cc(2,1,k) + ti2 + ti3

      cr2 = cc(1,1,k) + tr11 * tr2 + tr12 * tr3
      ci2 = cc(2,1,k) + tr11 * ti2 + tr12 * ti3
      cr3 = cc(1,1,k) + tr12 * tr2 + tr11 * tr3
      ci3 = cc(2,1,k) + tr12 * ti2 + tr11 * ti3

      cr5 = ti11 * tr5 + ti12 * tr4
      ci5 = ti11 * ti5 + ti12 * ti4
      cr4 = ti12 * tr5 - ti11 * tr4
      ci4 = ti12 * ti5 - ti11 * ti4

      ch(1,k,2) = cr2 - ci5
      ch(1,k,5) = cr2 + ci5
      ch(2,k,2) = ci2 + cr5
      ch(2,k,3) = ci3 + cr4
      ch(1,k,3) = cr3 - ci4
      ch(1,k,4) = cr3 + ci4
      ch(2,k,4) = ci3 - cr4
      ch(2,k,5) = ci2 - cr5

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        ti5 = cc(i,2,k) - cc(i,5,k)
        ti2 = cc(i,2,k) + cc(i,5,k)
        ti4 = cc(i,3,k) - cc(i,4,k)
        ti3 = cc(i,3,k) + cc(i,4,k)

        tr5 = cc(i-1,2,k) - cc(i-1,5,k)
        tr2 = cc(i-1,2,k) + cc(i-1,5,k)
        tr4 = cc(i-1,3,k) - cc(i-1,4,k)
        tr3 = cc(i-1,3,k) + cc(i-1,4,k)

        ch(i-1,k,1) = cc(i-1,1,k) + tr2 + tr3
        ch(i,k,1)   = cc(i,1,k)   + ti2 + ti3

        cr2 = cc(i-1,1,k) + tr11 * tr2 + tr12 * tr3
        ci2 = cc(i,1,k)   + tr11 * ti2 + tr12 * ti3
        cr3 = cc(i-1,1,k) + tr12 * tr2 + tr11 * tr3
        ci3 = cc(i,1,k)   + tr12 * ti2 + tr11 * ti3

        cr5 = ti11 * tr5 + ti12 * tr4
        ci5 = ti11 * ti5 + ti12 * ti4
        cr4 = ti12 * tr5 - ti11 * tr4
        ci4 = ti12 * ti5 - ti11 * ti4

        dr3 = cr3 - ci4
        dr4 = cr3 + ci4
        di3 = ci3 + cr4
        di4 = ci3 - cr4
        dr5 = cr2 + ci5
        dr2 = cr2 - ci5
        di5 = ci2 - cr5
        di2 = ci2 + cr5

        ch(i-1,k,2) = wa1(i-1) * dr2 + wa1(i) * di2
        ch(i,k,2)   = wa1(i-1) * di2 - wa1(i) * dr2
        ch(i-1,k,3) = wa2(i-1) * dr3 + wa2(i) * di3
        ch(i,k,3)   = wa2(i-1) * di3 - wa2(i) * dr3
        ch(i-1,k,4) = wa3(i-1) * dr4 + wa3(i) * di4
        ch(i,k,4)   = wa3(i-1) * di4 - wa3(i) * dr4
        ch(i-1,k,5) = wa4(i-1) * dr5 + wa4(i) * di5
        ch(i,k,5)   = wa4(i-1) * di5 - wa4(i) * dr5

      end do
    end do

  end if

  return
end

!=======================

subroutine fft4(data,npts)

!
!  Modified
!	German Prieto
!	November 2004
!
!  The discrete fourier tranform of a complex data set of size npts, 
!  working on real(4) data, instead of real(8). The code computes
!  everything in real(8), but returns simple real(4).
!
!  Discussion:
!
! 	Note here that we are working with double precision, to keep 
! 	the notation from the multitaper code accordingly. 
!	This FFT does not seem to have the limitations of other 
!	codes, in the sense that it is capable of computing FFTs
!      	for any size of the data. 
!
!  Input/Output 	
!
!	data	the complex data to be transformed, data(npts)
!
!  Input
!
!	npts	the length of the series data
!
!  calls cffti, cfftf
!

!**********************************************************************

   implicit none

   integer, intent(in) :: npts
   
   real(8), dimension(4*npts+15) :: wsave

   complex(8), dimension(npts) :: x
   complex(4), intent(in out), dimension(npts) :: data
   
!**********************************************************************

   call cffti(npts, wsave)
 
   x =  cmplx(data,kind=8)
   
   call cfftf(npts, x, wsave)

   data = cmplx(x,kind=4)

   return

end subroutine fft4

!--------------------------------------------------------------------

subroutine fft_2d(data,npts,nfft)

!
!  Modified
!	German Prieto
!	November 2004
!
!  The discrete fourier tranform of a complex data set of of Fourier 
!  coefficients of size npts, with nfft vectors in columns. The FFT 
!  is taken over the columns. 
!
!  Discussion:
!
! 	Note here that we are working with double precision, to keep 
! 	the notation from the multitaper code accordingly. 
!	This FFT does not seem to have the limitations of other 
!	codes, in the sense that it is capable of computing iFFTs
!      	for any size of the data. 
!
!  Input/Output 	
!
!	data	the complex data to be forward transformed, data(npts,nfft)
!
!  Input
!
!	npts	the length of the fourier coefficient data
!       nfft	the number of columns to the FFT's from.
!
!  calls cffti, cfftf
!

!**********************************************************************

   implicit none

   integer 	       :: i
   integer, intent(in) :: npts, nfft
   
   real(8), dimension(4*npts+15) :: wsave

   complex(8), intent(in out), dimension(npts,nfft) :: data
   
!**********************************************************************

   if (nfft>4000 .and. npts>4000) then
      write(6,'(a)') 'It may take a while'
   endif

   call cffti(npts, wsave)

   do i = 1,nfft
      call cfftf(npts, data(:,i), wsave)
   enddo
   
   return

end subroutine fft_2d



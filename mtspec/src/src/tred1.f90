subroutine tred1 ( n, a, d, e, e2 )
!
!*******************************************************************************
!
!! TRED1 transforms a real symmetric matrix to symmetric tridiagonal form.
!
!
!  Discussion:
!
!    The routine reduces a real symmetric matrix to a symmetric 
!    tridiagonal matrix using orthogonal similarity transformations.
!
!  Modified:
!
!    04 February 2003
!
!  Reference:
!
!    Martin, Reinsch, Wilkinson,
!    TRED1,
!    Numerische Mathematik,
!    Volume 11, pages 181-195, 1968.
!
!    J H Wilkinson and C Reinsch,
!    Handbook for Automatic Computation,
!    Volume II, Linear Algebra, Part 2,
!    Springer Verlag, 1971.
!
!    B Smith, J Boyle, J Dongarra, B Garbow, Y Ikebe, V Klema, C Moler,
!    Matrix Eigensystem Routines, EISPACK Guide,
!    Lecture Notes in Computer Science, Volume 6,
!    Springer Verlag, 1976.
!
!  Parameters:
!
!    Input, integer N, the order of the matrix A.
!
!    Input/output, real A(N,N), on input, contains the real symmetric matrix.  
!    Only the lower triangle of the matrix need be supplied.
!    On output, A contains information about the orthogonal transformations 
!    used in the reduction in its strict lower triangle.  
!    The full upper triangle of A is unaltered.
!
!    Output, real D(N), contains the diagonal elements of the tridiagonal 
!    matrix.
!
!    Output, real E(N), contains the subdiagonal elements of the tridiagonal
!    matrix in its last n-1 positions.  e(1) is set to zero.
!
!    Output, real E2(N), contains the squares of the corresponding 
!    elements of E.  E2 may coincide with E if the squares are not needed.
!

!********************************************************************

  implicit none

  integer :: n, i, ii, j, k, l

  real(8) :: f, g, h, scale

  real(8), dimension(n,n) :: a
  real(8), dimension(n)   :: d, e, e2
!
!********************************************************************

  d(1:n) = a(n,1:n)

  do i = 1, n
    a(n,i) = a(i,i)
  end do

  do ii = 1, n

    i = n + 1 - ii
    l = i - 1
    h = 0.0d0
!
!  Scale row.
!
    scale = sum ( abs ( d(1:l) ) )

    if ( scale == 0.0d0 ) then

      do j = 1, l
        d(j) = a(l,j)
        a(l,j) = a(i,j)
        a(i,j) = 0.0d0
      end do

      e(i) = 0.0d0
      e2(i) = 0.0d0

      cycle

    end if

    d(1:l) = d(1:l) / scale

    do k = 1, l
      h = h + d(k)**2
    end do

    e2(i) = h * scale**2
    f = d(l)
    g = - sign ( sqrt ( h ), f )
    e(i) = scale * g
    h = h - f * g
    d(l) = f - g

    if ( l >= 1 ) then
!
!  Form A * U.
!
      e(1:l) = 0.0d0

      do j = 1, l

        f = d(j)
        g = e(j) + a(j,j) * f

        do k = j+1, l
          g = g + a(k,j) * d(k)
          e(k) = e(k) + a(k,j) * f
        end do

        e(j) = g

      end do
!
!  Form P.
!
      f = 0.0d0

      do j = 1, l
        e(j) = e(j) / h
        f = f + e(j) * d(j)
      end do

      h = f / ( h + h )
!
!  Form Q.
!
      e(1:l) = e(1:l) - h * d(1:l)
!
!  Form reduced A.
!
      do j = 1, l

        f = d(j)
        g = e(j)

        a(j:l,j) = a(j:l,j) - f * e(j:l) - g * d(j:l)

      end do

    end if

    do j = 1, l
      f = d(j)
      d(j) = a(l,j)
      a(l,j) = a(i,j)
      a(i,j) = f * scale
    end do


  end do

  return

end subroutine tred1

!--------------------------------------------------------------------




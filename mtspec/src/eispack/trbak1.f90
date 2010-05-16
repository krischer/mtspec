subroutine trbak1 ( n, a, e, m, z ) 
!
!*******************************************************************************
!
!! TRBAK1 determines eigenvectors by undoing the TRED1 transformation.
!
!
!  Discussion:
!
!    This subroutine forms the eigenvectors of a real symmetric
!    matrix by back transforming those of the corresponding
!    symmetric tridiagonal matrix determined by TRED1.
!
!  Modified:
!
!    04 February 2003
!
!  Reference:
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
!    Input, integer N, the order of the matrix.
!
!    Input, real A(N,N), contains information about the orthogonal 
!    transformations used in the reduction by TRED1 in its strict lower
!    triangle.
!
!    Input, real E(N), the subdiagonal elements of the tridiagonal
!    matrix in E(2:N).  E(1) is arbitrary.
!
!    Input, integer M, the number of eigenvectors to be back transformed.
!
!    Input/output, real Z(N,M).  On input, the eigenvectors to be back 
!    transformed.  On output, the transformed eigenvectors.
!

!********************************************************************

  implicit none
!
  integer :: m, n, i, j, l

  real(8) :: s

  real(8), dimension(n,n) :: a
  real(8), dimension(n)   :: e
  real(8), dimension(n,m) :: z
!
!********************************************************************

  if ( m <= 0 ) then
    return
  end if

  if ( n <= 1 ) then
    return
  end if

  do i = 2, n

    l = i - 1

    if ( e(i) /= 0.0d0 ) then

      do j = 1, m

        s = dot_product ( a(i,1:l), z(1:l,j) )

        s = ( s / a(i,l) ) / e(i)

        z(1:l,j) = z(1:l,j) + s * a(i,1:l)

      end do

    end if

  end do

  continue

  return

end subroutine trbak1

!--------------------------------------------------------------------




module plot

!
!  Interface to allow for x or y to be a matrix, and plot each vector
!  of y against x. The matrix is given by horizontal vectors, y(:,1) is 
!  the first vector.
!	April 22 2005
!		Allowing to also use real(4) and real(8) units.
!

!
!  Interface
!      		dvv	vector vs vector
!		dvm	vector vs matrix
!		dmm	matrix vs matrix
!		rvv	real(4) vector vs vector
!		rvm	real(4) vector vs matrix
!		rmm	real(4) matrix vs matrix
!

   interface gnuplot
      module procedure gnuplot_dvv, gnuplot_dvm, gnuplot_dmm,              &
                       gnuplot_rvv, gnuplot_rvm, gnuplot_rmm,              &
                       gnuplot_rv,  gnuplot_rm		
   end interface gnuplot

!
!  The subroutines
!
   
   contains

!
!  The NEW GNUPlot subroutines
!

   include 'gnuplot.f90'

end module plot

!======================================================================





subroutine dpss_ev(ndata,nefn,w,atol,efn,evalu,theta)

!
!  Modified
!	German Prieto
!	November 2004
!
!   computes eigenvalues for the discrete prolate spheroidal sequences
!   in efn by integration of the corresponding squared discrete prolate
!   spheroidal wavefunctions over the inner domain. Due to symmetry, we
!   perform integration from zero to w. 
!   We use Chebychev quadrature for the numerical integration. 
! 
!   Input:
!
!      ndata  - actual number of time points in each column of efn
!      nefn   - number of dpss (= number of columns in efn)
!      w      - the bandwidth (= time-bandwidth product/ndata)
!      atol   - absolute error tolerance for the integration. this should
!               be set to 10**-n, where n is the number of significant figures
!               that can be be represented on the machine.
!      efn    - array containing the dpss
!
!   Output:
!
!      evalu  - vector of length nefn to contain the eigenvalues
!      theta - vector of length nefn to contain (1-eigenvalues)
! 
! 
!   calls functions xint
! 

!**********************************************************************

   implicit none

   
   integer, intent(in) :: ndata, nefn
   real(8), intent(in) :: atol,w
   real(8), dimension(ndata,nefn), intent(in) :: efn
   real(8), intent(out), dimension(nefn) :: evalu, theta

   integer :: k,i
   real(8) :: rup, rlow, sum
   
   real(8) :: result

   real(8) :: xint

!**********************************************************************
   
   do k=1,nefn
     

      result = xint(0.d0,w,atol,efn(1,k),ndata)

      
      evalu(k) = 2.d0*result
  
! 
!   calculate 1-lambda, explicitly if necessary
!

      if ((1.0d0-evalu(k)) .le. 1d-13) then

! 
!        break up 1-evalu calculation into reasonable pieces 
!        so xint can converge
! 

         sum=0.d0


         do i = 1,floor(.5d0/w)
            rlow = dble(i)*w
            if (rlow >= .5d0) then
               exit
            endif
            rup  = min(rlow+w,.5d0)
            result = xint(rlow,rup,atol,efn(1,k),ndata)
            sum=sum+result
         enddo

	 theta(k)=2.d0*sum
        
      else
      
         theta(k)=1.0d0-evalu(k)
      
      endif
     
   enddo

   return
      
end subroutine dpss_ev



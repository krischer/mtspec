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

   interface gplot
      module procedure gplot_dvv, gplot_dvm, gplot_dmm,              &
                       gplot_rvv, gplot_rvm, gplot_rmm,              &
                       gplot_rv,  gplot_rm		
   end interface gplot

!
!  The subroutines
!
   
   contains

   subroutine gplot_dvv( x, y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of the two vectors
!  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts
      character (len=64) :: splot 
      
      real(8), dimension(:), intent(in) :: x, y

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)

      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not that of X'
	 stop
      endif

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif

      write(2,'(a)') 'file *'

      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.d0) ) then
         write(2,'(a,i6)') 'read ', npts-1 
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=2,npts)
      else
         write(2,'(a,i6)') 'read ', npts 
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=1,npts)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
       
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      return

   end subroutine gplot_dvv

!--------------------------------------------------------------------

   subroutine gplot_dvm( x, y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of current spectrum, or of all
!  spectra.  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts, i, nlines
      character (len=64) :: splot 
      
      real(8), dimension(:), intent(in) :: x 
      real(8), dimension(:,:), intent(in) :: y

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)
      nlines = size(y,2)

      if (nlines > 100) then
         write(6,'(a)') 'Too many lines to plot, nlines > 100 '
	 stop
      endif


      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not appropiate to X'
	 stop
      endif

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif

      do i = 1,nlines
         write(2,'(a)') 'file *'
         write(2,'(a,i6)') 'read ', npts 

   
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k,i), k=1,npts)

      enddo

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
 
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      return

   end subroutine gplot_dvm

!--------------------------------------------------------------------

   subroutine gplot_dmm( x, y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of current spectrum, or of all
!  spectra.  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts, i, nlines
      character (len=64) :: splot 
      
      real(8), dimension(:,:), intent(in) :: x, y

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)
      nlines = size(y,2)

      if (nlines > 100) then
         write(6,'(a)') 'Too many lines to plot, nlines > 100 '
	 stop
      endif

      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not appropiate to X'
	 stop
      endif

      if (size(y,2) .ne. size(x,2)) then 
         write(6,'(a)') 'Error, the size of X and Y are not the same'
	 stop
      endif

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif

      do i = 1,nlines
         write(2,'(a)') 'file *'
         write(2,'(a,i6)') 'read ', npts 

   
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k,i), y(k,i), k=1,npts)

      enddo

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
       
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      return

   end subroutine gplot_dmm

!--------------------------------------------------------------------
! The real(4) versions
!--------------------------------------------------------------------

   subroutine gplot_rvv( x, y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of the two vectors
!  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts
      character (len=64) :: splot 
      
      real(4), dimension(:), intent(in) :: x, y

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)

      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not that of X'
	 stop
      endif

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif
 

      write(2,'(a)') 'file *'

      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         write(2,'(a,i6)') 'read ', npts-1 
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=2,npts)
      else
         write(2,'(a,i6)') 'read ', npts 
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=1,npts)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
     
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      return

   end subroutine gplot_rvv

!--------------------------------------------------------------------


   subroutine gplot_rvm( x, y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of current spectrum, or of all
!  spectra.  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts, i, nlines
      character (len=64) :: splot 
      
      real(4), dimension(:), intent(in) :: x 
      real(4), dimension(:,:), intent(in) :: y

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)
      nlines = size(y,2)

      if (nlines > 100) then
         write(6,'(a)') 'Too many lines to plot, nlines > 100 '
	 stop
      endif


      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not appropiate to X'
	 stop
      endif

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif

      do i = 1,nlines
         write(2,'(a)') 'file *'

         if (present(logxy) .and. index(logxy,'loglog') /= 0 &
             .and. (x(1)<=0.0) ) then
            write(2,'(a,i6)') 'read ', npts-1 
            write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k,i), k=2,npts)
         else
            write(2,'(a,i6)') 'read ', npts 
            write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k,i), k=1,npts)
         endif

      enddo

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
       
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      return

   end subroutine gplot_rvm

!--------------------------------------------------------------------

   subroutine gplot_rmm( x, y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of current spectrum, or of all
!  spectra.  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts, i, nlines
      character (len=64) :: splot 
      
      real(4), dimension(:,:), intent(in) :: x, y

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)
      nlines = size(y,2)

      if (nlines > 100) then
         write(6,'(a)') 'Too many lines to plot, nlines > 100 '
	 stop
      endif

      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not appropiate to X'
	 stop
      endif

      if (size(y,2) .ne. size(x,2)) then 
         write(6,'(a)') 'Error, the size of X and Y are not the same'
	 stop
      endif

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif

      do i = 1,nlines
         write(2,'(a)') 'file *'
         write(2,'(a,i6)') 'read ', npts 

   
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k,i), y(k,i), k=1,npts)

      enddo
      
      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
 
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      return

   end subroutine gplot_rmm

!--------------------------------------------------------------------
! The real(4) single vector versions
!--------------------------------------------------------------------

   subroutine gplot_rv( y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of the two vectors
!  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts, i
      character (len=64) :: splot 
      
      real(4), dimension(:), intent(in) :: y

      real(4), dimension(:), allocatable :: x

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(y,1)

      allocate(x(npts))

      x = real( (/(i-1, i=1,npts)/) )
 
      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif
 

      write(2,'(a)') 'file *'

      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         write(2,'(a,i6)') 'read ', npts-1 
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=2,npts)
      else
         write(2,'(a,i6)') 'read ', npts 
         write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=1,npts)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
     
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         deallocate(x)
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      deallocate(x)
      return

   end subroutine gplot_rv

!--------------------------------------------------------------------


   subroutine gplot_rm( y, hold, logxy, title, xlabel, ylabel, affine,   &
   				char, dash, frame,   &
				weight, output, smooth,      &
				symbol, xlimit, ylimit, color,fill)

!
!  calls system
!
!  Creates plotxy plotfile of current spectrum, or of all
!  spectra.  Invokes plotxy via 'system' call
!

!**********************************************************************

      implicit none

      integer :: k, npts, i, nlines
      character (len=64) :: splot 
      
      real(4), dimension(:,:), intent(in) :: y

      real(4), dimension(:), allocatable :: x 

      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: logxy, affine, char
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, smooth, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(y,1)
      nlines = size(y,2)

      if (nlines > 100) then
         write(6,'(a)') 'Too many lines to plot, nlines > 100 '
	 stop
      endif

      allocate(x(npts))

      x = real( (/(i-1, i=1,npts)/) )

      splot = 'pxy.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(title)) then
         write(2,'(2a)') 'title ', trim(title)
      endif

      if (present(xlabel)) then
         write(2,'(2a)') 'xlabel ', trim(xlabel)
      endif

      if (present(ylabel)) then
         write(2,'(2a)') 'ylabel ', trim(ylabel)
      endif

      if (present(logxy)) then
         write(2,'(2a)') 'logxy ', trim(logxy)
      endif

      if (present(frame)) then
         write(2,'(2a)') 'frame ', trim(frame)
      endif

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(char)) then
         write(2,'(2a)') 'character ', trim(char)
      endif

      if (present(smooth)) then
         write(2,'(2a)') 'smooth ', trim(smooth)
      endif

      if (present(dash)) then
         if (index(dash,'--') /= 0) then
	    write(2,'(a)') 'dash 0.2, 0.1'
	 elseif (index(dash,'..') /= 0) then
	    write(2,'(a)') 'dash 0.01, 0.05'
	 else
            write(2,'(2a)') 'dash ', trim(dash)
	 endif
      endif

      if (present(symbol)) then
         write(2,'(2a)') 'symbol ', trim(symbol)
      endif

      if (present(xlimit)) then
         write(2,'(2a)') 'xlimit ', trim(xlimit)
      endif

      if (present(ylimit)) then
         write(2,'(2a)') 'ylimit ', trim(ylimit)
      endif

      if (present(weight)) then
         write(2,'(2a)') 'weight ', trim(weight)
      endif

      if (present(color)) then
         write(2,'(2a)') 'color ', trim(color)
      endif

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 11
         write(2,'(a)') 'fill'
      endif

      do i = 1,nlines
         write(2,'(a)') 'file *'

         if (present(logxy) .and. index(logxy,'loglog') /= 0 &
             .and. (x(1)<=0.0) ) then
            write(2,'(a,i6)') 'read ', npts-1 
            write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k,i), k=2,npts)
         else
            write(2,'(a,i6)') 'read ', npts 
            write(2,'(1p,g13.6,1x,g13.6)')  (x(k), y(k,i), k=1,npts)
         endif

      enddo

      if (present(fill)) then
         write(2,'(a,i5)') 'color ', 1
      endif
       
      if (present(hold)) then
         if (index(hold,'plot') /= 0) then
	    write(2,'(a)') trim(hold)
         endif
         deallocate(x)
         return
      endif

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Plotxy file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(plotxy < '//splot//' > /dev/null)')

      if (present(output)) then
         call system('mv mypost '//trim(output)//' ')	
         call system('gv '//trim(output)//'&') 
      else
         call system('mv mypost newplot.ps')
         call system('gv newplot.ps&')
      endif
      
      call system('sleep 5')

      deallocate(x)
      return

   end subroutine gplot_rm


!--------------------------------------------------------------------
! Color Plots
!--------------------------------------------------------------------

   subroutine gcolor( x, affine, axes, border, height,   &
   				width, interval, label,   &
				landscape, level, nobar,      &
				orient, palette, output )

!
!  calls system
!
!  Creates color plotfile of the matrix
!  Invokes color via 'system' call
!

!**********************************************************************

      implicit none

      integer :: m, n, i
      character (len=64)  :: splot
      character (len=100) :: fmt 
      
      real(4), dimension(:,:), intent(in) :: x

      character (len=*), intent(in), optional :: affine, axes, border
      character (len=*), intent(in), optional :: height, width, interval
      character (len=*), intent(in), optional :: label, landscape
      character (len=*), intent(in), optional :: level, nobar
      character (len=*), intent(in), optional :: orient, palette
      character (len=*), optional :: output

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      m = size(x,1)
      n = size(x,2)

      splot = 'pcol.txt'

      open(2,file=splot)
   
!  Write plotfile to unit 2

      if (present(affine)) then
         write(2,'(2a)') 'affine ', trim(affine)
      endif

      if (present(axes)) then
         write(2,'(2a)') 'axes ', trim(axes)
      endif

      if (present(border)) then
         write(2,'(2a)') 'border ', trim(border)
      endif

      if (present(height)) then
         write(2,'(2a)') 'height ', trim(height)
      endif

      if (present(width)) then
         write(2,'(2a)') 'width ', trim(width)
      endif

      if (present(interval)) then
         write(2,'(2a)') 'interval ', trim(interval)
      endif

      if (present(label)) then
         write(2,'(2a)') 'label ', trim(label)
      endif

      if (present(landscape)) then
         write(2,'(2a)') 'landscape '
      endif

      if (present(level)) then
         write(2,'(2a)') 'level ', trim(level)
      endif

      if (present(nobar)) then
         write(2,'(2a)') 'nobar '
      endif

      if (present(orient)) then
         write(2,'(2a)') 'orient ', trim(orient)
      endif

      if (present(palette)) then
         write(2,'(2a)') 'palette ', trim(palette)
      else
         write(2,'(a)') 'palette 1'
      endif

      write(2,'(a)') 'file *'
      write(2,'(a,2i6)') 'read ', m, n 

      write(fmt,'(i12)') n
      fmt = '(' // trim(adjustl(fmt)) // 'E16.7)'
 
      do i = 1,m
         write(2,fmt)  x(i,:)
      enddo

!      write(2,'(a)') 'plot 0 0'
      write(2,'(a)') 'plot'
      write(2,'(a)') 'stop'
      close (2)

!  Finished the plot file

      write(6,'(/a80)')'Color file written to '//splot

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      if (m*n <= 250000) then
         call system('sleep 1')
         call system('(color2 < '//splot//' > /dev/null)')
         call system('sleep 5')

         if (present(output)) then
            call system('mv mypost '//trim(output)//' ')	
            call system('gv '//trim(output)//'&') 
         else
            call system('mv mypost newplot.ps')
            call system('gv newplot.ps&')
         endif
      else
         write(6,'(a)') 'Size of array too large to attemp plot'
         write(6,'(a)') 'Try saving matrix with SAVE command'
      endif

      
      return

   end subroutine gcolor

end module plot

!======================================================================




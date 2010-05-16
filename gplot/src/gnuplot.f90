   subroutine gnuplot_dvv( x, y, hold, logxy, title, xlabel, ylabel,   &
                           dash, frame,   &
                           weight, output,       &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound, i
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt

! Save values

      integer, save                      :: ignu
      character (len=120), dimension(20) :: plot_txt
      character (len=30),  dimension(20) :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)

      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not that of X'
         stop
      endif

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif

!  Write GNUPLOT commands to unit 2
   
      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,'(1p,g13.6,1x,g13.6)')  (real(x(k)), real(y(k)), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,'(1p,g13.6,1x,g13.6)')  (real(x(k)), real(y(k)), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu

!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
         else
            write(ptxt,'(5a)') '"',trim(file_txt(ignu)), & 
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
         endif
         plot_txt(ignu) = trim(adjustl(ptxt))
         return
      else
         if (ignu==1) then
            write(ptxt,'(4a)') 'plot "',trim(file_txt(ignu)),   &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot))
         else
            write(ptxt,'(4a)') '"',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot))
         endif
         plot_txt(ignu) = trim(adjustl(ptxt))
         do i = 1,ignu
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
      call system('rm gnu_data_*.dat gnu.exe')

      return

   end subroutine gnuplot_dvv

!--------------------------------------------------------------------

   subroutine gnuplot_dvm( x, y, hold, logxy, title, xlabel, ylabel,   &
                           dash, frame,   &
                           weight, output,       &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt
      character (len=40)        :: wrt_lines

! Save values

      integer, save                      :: ignu, imat
      character (len=120), dimension(99) :: plot_txt
      character (len=30),  dimension(99) :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         imat = 0
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
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

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif

!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      write(wrt_lines,'(1a,i3,1a)') '(1p,g13.6,1x,',nlines,'g13.6)'
      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (real(x(k)), real(y(k,:)), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (real(x(k)), real(y(k,:)), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu

!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         else
            do i = 1,nlines
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         endif
         return
      else
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ',  & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines-1
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,2a)') '"',trim(file_txt(ignu)),   &
                      '" using 1: ',nlines+1,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         else
            do i = 1,nlines-1
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1:', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,2a)') '"',trim(file_txt(ignu)),   &
                      '" using 1: ', nlines+1,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         endif

         if (imat > 99) then
            write(6,*) 'Too many lines to plot, stopped'
            stop
         endif

         do i = 1,imat
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
      call system('rm gnu_data_*.dat gnu.exe')

      return

   end subroutine gnuplot_dvm

!--------------------------------------------------------------------

   subroutine gnuplot_dmm( x, y, hold, logxy, title, xlabel, ylabel,    &
                           dash, frame,   &
                           weight, output,      &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt
      character (len=40)        :: wrt_lines

! Save values

      integer, save                      :: ignu, imat
      character (len=120), dimension(20) :: plot_txt
      character (len=30),  dimension(20) :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         imat = 0
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
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

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      write(wrt_lines,'(1a,i3,1a,i3,1a)') &
                      '(1p,',nlines,'g13.6,1x,',nlines,'g13.6)'
      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. any(x(1,:)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (real(x(k,:)), real(y(k,:)), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (real(x(k,:)), real(y(k,:)), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu

!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(3a,i3,1a,i3,3a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using ',1,':', 1+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         else
            do i = 1,nlines
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         endif
         return
      else
         if (ignu==1) then
            write(ptxt,'(3a,i3,1a,i3,3a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using ',1,':', 1+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines-1
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,1a,i3,2a)') '"',trim(file_txt(ignu)),  &
                      '" using ',nlines,':', 2*nlines,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         else
            do i = 1,nlines-1
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,1a,i3,2a)') '"',trim(file_txt(ignu)),  &
                      '" using ',nlines,':', 2*nlines,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         endif

         if (imat > 99) then
            write(6,*) 'Too many lines to plot, stopped'
            stop
         endif

         do i = 1,imat
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif

      call system('rm gnu_data_*.dat gnu.exe')

      return

   end subroutine gnuplot_dmm

!--------------------------------------------------------------------
! The real(4) versions
!--------------------------------------------------------------------

   subroutine gnuplot_rvv( x, y, hold, logxy, title, xlabel, ylabel,    &
                           dash, frame,   &
                           weight, output,       &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound, i
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt

! Save values

      integer, save                      :: ignu
      character (len=120), dimension(20) :: plot_txt
      character (len=30),  dimension(20) :: file_txt


!**********************************************************************

      if (ignu==0) then
         ignu = 1
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(x,1)

      if (size(y,1) .ne. npts) then
         write(6,'(a)') 'Error, the size of Y is not that of X'
         stop
      endif

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif

!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif


! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu

!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
         else
            write(ptxt,'(5a)') '"',trim(file_txt(ignu)), & 
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
         endif
         plot_txt(ignu) = trim(adjustl(ptxt))
         return
      else
         if (ignu==1) then
            write(ptxt,'(4a)') 'plot "',trim(file_txt(ignu)),   &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot))
         else
            write(ptxt,'(4a)') '"',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot))
         endif
         plot_txt(ignu) = trim(adjustl(ptxt))
         do i = 1,ignu
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
!      call system('rm gnu_data_*.dat gnu.exe')

      return

   end subroutine gnuplot_rvv

!--------------------------------------------------------------------


   subroutine gnuplot_rvm( x, y, hold, logxy, title, xlabel, ylabel,    &
                           dash, frame,   &
                           weight, output,       &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt
      character (len=40)        :: wrt_lines

! Save values

      integer, save                      :: ignu, imat
      character (len=120), dimension(20) :: plot_txt
      character (len=30),  dimension(20) :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         imat = 0
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
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

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      write(wrt_lines,'(1a,i3,1a)') '(1p,g13.6,1x,',nlines,'g13.6)'
      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (x(k), y(k,:), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (x(k), y(k,:), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu
       
!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines
               write(ptxt,'(3a,i2,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1, ' ',& 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         else
            do i = 1,nlines
               write(ptxt,'(3a,i2,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         endif
         return
      else
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ',  & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines-1
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,2a)') '"',trim(file_txt(ignu)),   &
                      '" using 1: ',nlines+1,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         else
            do i = 1,nlines-1
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1:', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,2a)') '"',trim(file_txt(ignu)),   &
                      '" using 1: ', nlines+1,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         endif

         if (imat > 99) then
            write(6,*) 'Too many lines to plot, stopped'
            stop
         endif

         do i = 1,imat
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif


!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
      call system('rm gnu_data_*.dat gnu.exe')

      return

   end subroutine gnuplot_rvm

!--------------------------------------------------------------------

   subroutine gnuplot_rmm( x, y, hold, logxy, title, xlabel, ylabel,   &
                           dash, frame,   &
                           weight, output,       &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt
      character (len=40)        :: wrt_lines

! Save values

      integer, save                      :: ignu, imat
      character (len=120), dimension(20) :: plot_txt
      character (len=30),  dimension(20) :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         imat = 0
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
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

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      write(wrt_lines,'(1a,i3,1a,i3,1a)') &
                      '(1p,',nlines,'g13.6,1x,',nlines,'g13.6)'
      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. any(x(1,:)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (x(k,:), y(k,:), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (x(k,:), y(k,:), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu

!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(3a,i3,1a,i3,3a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using ',1,':', 1+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         else
            do i = 1,nlines
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         endif
         return
      else
         if (ignu==1) then
            write(ptxt,'(3a,i3,1a,i3,3a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using ',1,':', 1+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines-1
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,1a,i3,2a)') '"',trim(file_txt(ignu)),  &
                      '" using ',nlines,':', 2*nlines,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         else
            do i = 1,nlines-1
               write(ptxt,'(3a,i3,1a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using ',i,':', i+nlines,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,1a,i3,2a)') '"',trim(file_txt(ignu)),  &
                      '" using ',nlines,':', 2*nlines,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         endif

         if (imat > 99) then
            write(6,*) 'Too many lines to plot, stopped'
            stop
         endif

         do i = 1,imat
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
      call system('rm gnu_data_*.dat gnu.exe')

      return

   end subroutine gnuplot_rmm

!--------------------------------------------------------------------
! The real(4) single vector versions
!--------------------------------------------------------------------

   subroutine gnuplot_rv( y, hold, logxy, title, xlabel, ylabel,    &
                          dash, frame,   &
                          weight, output,      &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt

! Save values

      integer, save                      :: ignu
      character (len=120), dimension(20), save :: plot_txt
      character (len=30),  dimension(20), save :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
!
!  For now, save file as something fixed (overwrite previous)
!

      npts = size(y,1)

      allocate(x(npts))

      x = real( (/(i-1, i=1,npts)/) )
 
      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif
 
! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,'(1p,g13.6,1x,g13.6)')  (x(k), y(k), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu

!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif

!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
         else
            write(ptxt,'(5a)') '"',trim(file_txt(ignu)), & 
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
         endif
         plot_txt(ignu) = trim(adjustl(ptxt))
         return
      else
         if (ignu==1) then
            write(ptxt,'(4a)') 'plot "',trim(file_txt(ignu)),   &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot))
         else
            write(ptxt,'(4a)') '"',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot))
         endif
         plot_txt(ignu) = trim(adjustl(ptxt))
         do i = 1,ignu
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')


      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
      call system('rm gnu_data_*.dat gnu.exe')

      deallocate(x)
      return

   end subroutine gnuplot_rv

!--------------------------------------------------------------------


   subroutine gnuplot_rm( y, hold, logxy, title, xlabel, ylabel,   &
                          dash, frame,   &
                          weight, output,       &
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
      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: dash
      character (len=*), intent(in), optional :: frame, weight
      character (len=*), intent(in), optional :: color, symbol
      character (len=*), intent(in), optional :: xlimit, ylimit, hold
      character (len=*), intent(in), optional :: fill
      character (len=*), optional :: output

      character (len=10) :: lw, pt
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt
      character (len=40)        :: wrt_lines

! Save values

      integer, save                      :: ignu, imat
      character (len=120), dimension(20) :: plot_txt
      character (len=30),  dimension(20) :: file_txt

!**********************************************************************

      if (ignu==0) then
         ignu = 1
         imat = 0
         plot_txt = ''
         file_txt = ''
      else
         ignu = ignu + 1
      endif
      
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

      splot = 'gnu.exe'

      open(2,file=splot)
      if (ignu == 1) then
         write(2,'(2a)') 'gnuplot << EOF' 
      endif
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

! Define line attributes

      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
             write(pt,'(a)') 'pt 1'
         else
            write(pt,'(2a)') 'pt ',symbol
         endif
      else
         write(pt,'(a)') 'pt 1'
      endif

     if (present(dash)) then
         if (index(dash,'--') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 2 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         elseif (index(dash,'..') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 3 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'-.') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 5 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt)
         elseif (index(dash,'0,0') /= 0) then
            write(2,'(1a,i2,6a)') 'set style line ',ignu,  &
                                  ' lt 1 ', trim(lc),' ',  &
                                  trim(lw), ' ', trim(pt) 
         else
            write(2,'(1a,i2,8a)') 'set style line ',ignu,  &
                                  ' lt ', trim(dash),' ',  &
                                  trim(lc),' ', trim(lw), ' ', trim(pt)
         endif
      else
         write(2,'(1a,i2,6a)') 'set style line ',ignu,     &
                               ' lt 1 ', trim(lc),' ', trim(lw),' ',trim(pt)
      endif

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu_myplot.eps"'

!  Generate file names

      if (ignu<10) then
         write(file_txt(ignu),'(1a,i1,1a)') 'gnu_data_',ignu,'.dat'
      else
         write(file_txt(ignu),'(1a,i2,1a)') 'gnu_data_',ignu,'.dat'
      endif

      write(wrt_lines,'(1a,i3,1a)') '(1p,g13.6,1x,',nlines,'g13.6)'
      if (present(logxy) .and. index(logxy,'loglog') /= 0 &
          .and. (x(1)<=0.0) ) then
         print *, 'loglog plot'
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (x(k), y(k,:), k=2,npts)
         close(3)
      else
         open(3,file=trim(adjustl(file_txt(ignu)))) 
         write(3,wrt_lines)  (x(k), y(k,:), k=1,npts)
         close(3)
      endif

!  Standard, plot with lines

      write(type_plot,'(a,i2)') 'with lines ls ',ignu
       
!  Filled curve
      if (present(fill)) then
         if (index(fill,'abov') /= 0) then
            write(type_plot,'(a,i2)')  &
                    'with filledcurves above y1=0.0 ls ',ignu
         elseif (index(fill,'belo') /= 0) then
            write(type_plot,'(a,i2)')  & 
                    'with filledcurves below y1=0.0 ls ',ignu
         endif
      endif
     
!  Symbols
      if (present(symbol)) then
         if (index(symbol,'-1,0') /= 0) then
            type_plot = adjustl(type_plot)
         else
            write(type_plot,'(a,i2)') 'with points ls ',ignu
         endif
      endif

!     No legend
      write(2,'(a)') 'set key off'

!  Define the kind of plot (dots, lines, filled curve, etc)

      if (present(hold)) then
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ', & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines
               write(ptxt,'(3a,i2,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1, ' ',& 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         else
            do i = 1,nlines
               write(ptxt,'(3a,i2,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
         endif
         return
      else
         if (ignu==1) then
            write(ptxt,'(5a)') 'plot "',trim(file_txt(ignu)),  &
                      '" using 1:2 ',  & 
                      trim(adjustl(type_plot)),' ,\'
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
            do i = 2,nlines-1
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1: ', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,2a)') '"',trim(file_txt(ignu)),   &
                      '" using 1: ',nlines+1,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         else
            do i = 1,nlines-1
               write(ptxt,'(3a,i3,3a)') '"',trim(file_txt(ignu)),  &
                      '" using 1:', i+1,' ', & 
                      trim(adjustl(type_plot)),' ,\'
               imat = imat + 1
               plot_txt(imat) = trim(adjustl(ptxt))
            enddo
            write(ptxt,'(3a,i3,2a)') '"',trim(file_txt(ignu)),   &
                      '" using 1: ', nlines+1,' ', & 
                      trim(adjustl(type_plot))
            imat = imat + 1
            plot_txt(imat) = trim(adjustl(ptxt))
         endif

         if (imat > 99) then
            write(6,*) 'Too many lines to plot, stopped'
            stop
         endif

         do i = 1,imat
            write(2,'(a)') trim(adjustl(plot_txt(i)))
         enddo
!        Finish gnuplot file
         write(2,'(a)') 'quit'
         write(2,'(a)') 'EOF'
         close (2)
         ignu = 0   ! Last plot, erase memory
      endif

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke gnuplot, save file and close gnuplot
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')
      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot.eps'
         else
            output2 = output
         endif
         call system('mv gnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu_myplot.eps newplot.eps')
         call system('gv newplot.eps &')
      endif
      
      call system('rm gnu_data_*.dat gnu.exe')

      deallocate(x)
      return

   end subroutine gnuplot_rm


!--------------------------------------------------------------------
! 3d Plots
!--------------------------------------------------------------------

   subroutine gnucolor( x, logxy, logz, xaxes, yaxes, frame, height,       &
                        width, interval,                      &
                        nobar, title, xlabel, ylabel,   &
                        palette, output,  &
                        xlimit, ylimit, zlimit )

!
!  calls system
!
!  Creates color plotfile of the matrix
!  Invokes color via 'system' call
!

!**********************************************************************

      implicit none

      integer :: m, n, i
      character (len=64)  :: splot, splot2
      character (len=100) :: fmt 
      
      real(4), dimension(:,:), intent(in) :: x


      character (len=*), intent(in), optional :: logxy, logz
      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: xaxes, yaxes, frame
      character (len=*), intent(in), optional :: height, width, interval
      character (len=*), intent(in), optional :: nobar
      character (len=*), intent(in), optional :: palette
      character (len=*), optional :: output
      character (len=*), intent(in), optional :: xlimit, ylimit, zlimit

      integer                   :: nfound, ios
      real(4), dimension(2)     :: values
      real(4)                   :: sfac, sjump, gheight, gwidth

      character (len=120)       :: xtxt, ytxt, axis_txt
      character (len=50)        :: output2

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      m = size(x,1)
      n = size(x,2)

      splot = 'cgnu.exe'
      splot2 = 'cgnu_data.dat'

      open(2,file=splot)
      write(2,'(2a)') 'gnuplot << EOF' 

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "cgnu_myplot.eps"'
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 1023-128'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         endif
      endif


!  Define relative sizes of the plot axes
      if (present(height)) then
         read (height, *, iostat=ios) gheight
      else
         gheight = 1.0
      endif
      if (present(width)) then
         read (width, *, iostat=ios) gwidth
      else
         gwidth = 1.0
      endif
      gwidth = gwidth/max(gwidth,gheight)
      gheight = gheight/max(gwidth,gheight)
      write(2,*) 'set size ',gwidth,',', gheight 


!  Use predefined palette colors
      if (present(palette)) then
         if (index(palette,'seis') /= 0) then
            write(2,'(2a)') 'set palette defined ',   &
                            '(-1 "blue", 0 "white", 1 "red")'
            write(2,'(1a,1E16.7,1a,1E16.7,1a)') & 
                    'set cbrange [',-maxval(abs(x)) ,  &
                    ':',maxval(abs(x)) , ']'
         else
            write(2,'(a)') 'set palette gray'
         endif
      else
         write(2,'(a)') 'set palette gray'
      endif

!
! Define the axis limits, by scaling the index of the matrix
! to user defined limits.
!

      xtxt = '1'
      if (present(xaxes)) then
         call getlims(xaxes, values, nfound)
         if (nfound == 2) then
            sfac  = (values(2)-values(1))/real(n-1)
            sjump = values(1) 
            write(xtxt,*) '(\$1*',sfac,'+',sjump,')'
         endif
      endif

      ytxt = '2'
      if (present(yaxes)) then
         call getlims(yaxes, values, nfound)
         if (nfound == 2) then
            sfac  = (values(2)-values(1))/real(m-1)
            sjump = values(1) 
            write(ytxt,*) '(\$2*',sfac,'+',sjump,')'
         endif
      endif
   
      write(axis_txt,'(5a)') 'using ',trim(adjustl(xtxt)),':', &
                             trim(adjustl(ytxt)),':3'

!
!  Define x, y, and color limits
!
      
      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(zlimit)) then
         call getlims(zlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set cbrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(logz)) then
         if (index(logz,'log') /= 0) then
            write(2,'(2a)') 'set log cb'
         elseif (index(logz,'lin') /= 0) then
            write(2,'(2a)') 'unset log cb'
         endif
      endif

      if (present(nobar)) then
         write(2,'(2a)') 'unset colorbox '
      endif


      open(12,file=splot2)

      write(fmt,'(i12)') n
      fmt = '(' // trim(adjustl(fmt)) // 'E16.7)'
 
      do i = 1,m
         write(12,fmt)  x(i,:)
      enddo
      close(12)

      write(2,'(a)')  'set view map'
      write(2,'(a)')  'set pm3d' 
      write(2,'(a)')  'unset surface'
      write(2,'(2a)') 'splot "cgnu_data.dat" matrix ',trim(adjustl(axis_txt)) 
      write(2,'(a)')  'quit'
      write(2,'(a)')  'EOF'
      close (2)

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newcplot.eps'
         else
            output2 = output
         endif
         call system('mv cgnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv cgnu_myplot.eps newcplot.eps')
         call system('gv newcplot.eps &')
      endif

      call system('rm cgnu_data*.dat cgnu.exe')
      
      return

   end subroutine gnucolor

!--------------------------------------------------------------------

   subroutine gnucontour( x, logxy, logz, xaxes, yaxes, frame, height,   &
                        width, interval, weight, color,    &
                        level, title, xlabel, ylabel,   &
                        output, xlimit, ylimit, zlimit )

!
!  calls system
!
!  Creates color plotfile of the matrix
!  Invokes color via 'system' call
!

!**********************************************************************

      implicit none

      integer :: m, n, i
      character (len=64)  :: splot, splot2
      character (len=100) :: fmt 
      
      real(4), dimension(:,:), intent(in) :: x

      character (len=*), intent(in), optional :: logxy, logz, weight
      character (len=*), intent(in), optional :: height, width, interval
      character (len=*), intent(in), optional :: xaxes, yaxes, frame
      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: level, color
      character (len=*), optional :: output
      character (len=*), intent(in), optional :: xlimit, ylimit, zlimit

      character (len=10) :: lw
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

      integer                   :: nfound
      real(4), dimension(2)     :: values

      character (len=120)       :: ptxt


!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      m = size(x,1)
      n = size(x,2)

      splot = 'cgnu.exe'
      splot2 = 'cgnu_data.dat'

      open(2,file=splot)
      write(2,'(2a)') 'gnuplot << EOF' 

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "cgnu_myplot.eps"'
   

!  Write GNUPLOT commands to unit 2
   
      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 1023-128'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'set border' 
         endif
      endif

      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
         write(2,'(2a)') 'unset clabel'
      else
         write(lc,'(a)') 'lc 0'
      endif

! Set the levels of countours

      write(2,'(a)')  'set cntrparam levels 10'

      if (present(level)) then
         write(2,'(2a)') 'set cntrparam levels discrete ', trim(level)
      endif

      if (present(interval)) then
         write(2,'(2a)') 'set cntrparam levels incr ', trim(interval)
      endif

!  Save matrix file

      open(12,file=splot2)

      write(fmt,'(i12)') n
      fmt = '(' // trim(adjustl(fmt)) // 'E16.7)'
 
      do i = 1,m
         write(12,fmt)  x(i,:)
      enddo
      close(12)

!     No legend
      write(2,'(a)') 'set key off'

!  Standard, plot with lines

      write(2,'(1a,4a)') 'set style line 1',     &
                               ' lt 1 ', trim(lc),' ', trim(lw)

      write(type_plot,'(a,i2)') 'with lines ls 1'

      write(2,'(a)')  'set view map'
      write(2,'(a)')  'set contour'
      write(2,'(a)')  'unset surface'
      write(2,'(2a)') 'splot "cgnu_data.dat" matrix ', & 
                      trim(adjustl(type_plot))
      write(2,'(a)')  'quit'
      write(2,'(a)')  'EOF'
      close (2)

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newcplot.eps'
         else
            output2 = output
         endif
         call system('mv cgnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv cgnu_myplot.eps newcplot.eps')
         call system('gv newcplot.eps &')
      endif
      
      call system('rm cgnu_data*.dat cgnu.exe')
      
      return

   end subroutine gnucontour

!--------------------------------------------------------------------
! 3d Plots
!--------------------------------------------------------------------

   subroutine gnuvector( x, logxy, frame, height,       &
                        width, interval,                      &
                        title, xlabel, ylabel,   &
                        output, weight,  &
                        xlimit, ylimit, color )

!
!  calls system
!
!  Creates color plotfile of the matrix
!  Invokes color via 'system' call
!

!**********************************************************************

      implicit none

      integer :: m, n, i
      character (len=64)  :: splot, splot2
      character (len=100) :: fmt 
      
      real(4), dimension(:,:), intent(in) :: x


      character (len=*), intent(in), optional :: logxy
      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: frame, weight, color
      character (len=*), intent(in), optional :: height, width, interval
      character (len=*), optional             :: output
      character (len=*), intent(in), optional :: xlimit, ylimit

      integer                   :: nfound, ios
      real(4), dimension(2)     :: values
      real(4)                   :: sfac, sjump, gheight, gwidth

      character (len=120)       :: xtxt, ytxt, axis_txt

      real(4), dimension(:), allocatable :: vx, vy

      character (len=10) :: lw
      character (len=20) :: lc
      character (len=50) :: type_plot, output2

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      m = size(x,1)
      n = size(x,2)

      if ( n/=4 ) then
         write(6,*) 'Matrix should have ONLY 4 columns'
      endif

      allocate(vx(m),vy(m))

      splot = 'vgnu.exe'
      splot2 = 'vgnu_data.dat'

      open(2,file=splot)
      write(2,'(2a)') 'gnuplot << EOF' 

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "vgnu_myplot.eps"'
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 3'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         endif
      endif


!  Define relative sizes of the plot axes
      if (present(height)) then
         read (height, *, iostat=ios) gheight
      else
         gheight = 1.0
      endif
      if (present(width)) then
         read (width, *, iostat=ios) gwidth
      else
         gwidth = 1.0
      endif
      gwidth = gwidth/max(gwidth,gheight)
      gheight = gheight/max(gwidth,gheight)
      write(2,*) 'set size ',gwidth,',', gheight 

      if (present(weight)) then
         if (index(weight,'12,12') /= 0) then
            write(lw,'(a)') 'lw 2'
         else
            write(lw,'(2a)') 'lw ', trim(weight)
         endif
      else
         write(lw,'(a)') 'lw 2'
      endif

      if (present(color)) then
         if (index(color,'bla') /= 0) then
            write(lc,'(a)') 'lc 0'
         elseif ( index('0 1 2 3 4 5 6 7 8 9', trim(color)) /= 0) then
            write(lc,'(2a)') 'lc ', trim(color)
         else
            write(lc,'(3a)') 'lc rgb "', trim(color),'"'
         endif
      else
         write(lc,'(a)') 'lc 0'
      endif

      write(2,'(1a,4a)') 'set style line 1',     &
                               ' lt 1 ', trim(lc),' ', trim(lw)

! Scale vectors so that they don't overlap too heavily

      if (maxval(abs(x(:,3)))>0.0) then
         vx = x(:,3)/maxval(x(:,3))*(maxval(x(:,1))-minval(x(:,1)))/100.
      else
         vx = x(:,3)
      endif
      if (maxval(abs(x(:,4)))>0.0) then
         vy = x(:,4)/maxval(x(:,4))*(maxval(x(:,2))-minval(x(:,2)))/100.
      else
         vy = x(:,4)
      endif


      write(axis_txt,'(2a)') 'using 1:2',':3:4'

!
!  Define x, y, and color limits
!
      
      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      open(12,file=splot2)

      do i = 1,m
         write(12,'(4E16.7)')  x(i,1:2), vx(i), vy(i)
      enddo
      close(12)

!     No legend
      write(2,'(a)') 'set key off'

!  Standard, plot with lines

      write(type_plot,'(a)') 'with vec ls 1'

!  Command to plot vectors

      write(2,'(4a)') 'plot "vgnu_data.dat" ',    &
                       trim(adjustl(axis_txt)), ' ',trim(adjustl(type_plot)) 
      write(2,'(a)')  'quit'
      write(2,'(a)')  'EOF'
      close (2)

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newvplot.eps'
         else
            output2 = output
         endif
         call system('mv vgnu_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv vgnu_myplot.eps newvplot.eps')
         call system('gv newvplot.eps &')
      endif
      
!      call system('rm vgnu_data.dat vgnu.exe')
     
      deallocate(vx,vy)
 
      return

   end subroutine gnuvector

!--------------------------------------------------------------------
! 3d Plots
!--------------------------------------------------------------------

   subroutine gnuplot3( x, logxy, logz, xaxes, yaxes, frame, height,       &
                        width, interval,                      &
                        nobar, title, xlabel, ylabel,   &
                        palette, output,  &
                        xlimit, ylimit, zlimit )

!
!  calls system
!
!  Creates color plotfile of the matrix
!  Invokes color via 'system' call
!

!**********************************************************************

      implicit none

      integer :: m, n, i
      character (len=64)  :: splot, splot2
      character (len=100) :: fmt 
      
      real(4), dimension(:,:), intent(in) :: x


      character (len=*), intent(in), optional :: logxy, logz
      character (len=*), intent(in), optional :: title, xlabel, ylabel
      character (len=*), intent(in), optional :: xaxes, yaxes, frame
      character (len=*), intent(in), optional :: height, width, interval
      character (len=*), intent(in), optional :: nobar
      character (len=*), intent(in), optional :: palette
      character (len=*), optional :: output
      character (len=*), intent(in), optional :: xlimit, ylimit, zlimit

      integer                   :: nfound, ios
      real(4), dimension(2)     :: values
      real(4)                   :: sfac, sjump, gheight, gwidth

      character (len=120)       :: xtxt, ytxt, axis_txt
      character (len=50)        :: output2

!**********************************************************************

!
!  For now, save file as something fixed (overwrite previous)
!

      m = size(x,1)
      n = size(x,2)

      splot = 'gnu3.exe'
      splot2 = 'gnu3_data.dat'

      open(2,file=splot)
      write(2,'(2a)') 'gnuplot << EOF' 

      write(2,'(a)') 'set term post eps color enh "Helvetica" 14'
      write(2,'(a)') 'set output "gnu3_myplot.eps"'
   
!  Write GNUPLOT commands to unit 2

      if (present(title)) then
         write(2,'(3a)') 'set title "', trim(title),'"'
      endif

      if (present(xlabel)) then
         write(2,'(3a)') 'set xlabel "',trim(xlabel),'"'
      endif

      if (present(ylabel)) then
         write(2,'(3a)') 'set ylabel "',trim(ylabel),'"'
      endif

      if (present(frame)) then
         if (index(frame,'-box') /= 0) then
            write(2,'(1a)') 'set border 1023-128'
            write(2,'(1a)') 'set xtics nomirror'
            write(2,'(1a)') 'set ytics nomirror'
         elseif (index(frame,'none') /= 0) then
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         elseif (index(frame,'+box') /= 0) then
            write(2,'(1a)') 'set border' 
         else
            write(2,'(1a)') 'unset border' 
            write(2,'(1a)') 'unset xtics'
            write(2,'(1a)') 'unset ytics'
         endif
      endif


!  Define relative sizes of the plot axes
      if (present(height)) then
         read (height, *, iostat=ios) gheight
      else
         gheight = 1.0
      endif
      if (present(width)) then
         read (width, *, iostat=ios) gwidth
      else
         gwidth = 1.0
      endif
      gwidth = gwidth/max(gwidth,gheight)
      gheight = gheight/max(gwidth,gheight)
      write(2,*) 'set size ',gwidth,',', gheight 


!  Use predefined palette colors
      if (present(palette)) then
         if (index(palette,'seis') /= 0) then
            write(2,'(2a)') 'set palette defined ',   &
                            '(-1 "blue", 0 "white", 1 "red")'
            write(2,'(1a,1E16.7,1a,1E16.7,1a)') & 
                    'set cbrange [',-maxval(abs(x)) ,  &
                    ':',maxval(abs(x)) , ']'
         else
            write(2,'(a)') 'set palette gray'
         endif
      else
         write(2,'(a)') 'set palette gray'
      endif

!
! Define the axis limits, by scaling the index of the matrix
! to user defined limits.
!

      xtxt = '1'
      if (present(xaxes)) then
         call getlims(xaxes, values, nfound)
         if (nfound == 2) then
            sfac  = (values(2)-values(1))/real(n-1)
            sjump = values(1) 
            write(xtxt,*) '(\$1*',sfac,'+',sjump,')'
         endif
      endif

      ytxt = '2'
      if (present(yaxes)) then
         call getlims(yaxes, values, nfound)
         if (nfound == 2) then
            sfac  = (values(2)-values(1))/real(m-1)
            sjump = values(1) 
            write(ytxt,*) '(\$2*',sfac,'+',sjump,')'
         endif
      endif
   
      write(axis_txt,'(5a)') 'using ',trim(adjustl(xtxt)),':', &
                             trim(adjustl(ytxt)),':3'

!
!  Define x, y, and color limits
!
      
      if (present(xlimit)) then
         call getlims(xlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set xrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(ylimit)) then
         call getlims(ylimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set yrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(zlimit)) then
         call getlims(zlimit, values, nfound)
         if (nfound == 2) then
            write(2,*)  &
                    'set cbrange [',values(1),':',values(2),']'
         endif
      endif

      if (present(logxy)) then
         if (index(logxy,'loglog') /= 0) then
            write(2,'(2a)') 'set logscale xy'
         elseif (index(logxy,'linlog') /= 0) then
            write(2,'(2a)') 'set logscale y'
         elseif (index(logxy,'loglin') /= 0) then
            write(2,'(2a)') 'set logscale x'
         endif
      endif

      if (present(logz)) then
         if (index(logz,'log') /= 0) then
            write(2,'(2a)') 'set log cb'
         elseif (index(logz,'lin') /= 0) then
            write(2,'(2a)') 'unset log cb'
         endif
      endif

      if (present(nobar)) then
         write(2,'(2a)') 'unset colorbox '
      endif


      open(12,file=splot2)

      write(fmt,'(i12)') n
      fmt = '(' // trim(adjustl(fmt)) // 'E16.7)'
 
      do i = 1,m
         write(12,fmt)  x(i,:)
      enddo
      close(12)

      write(2,'(2a)') 'splot "gnu3_data.dat" matrix with lines '
      write(2,'(a)')  'quit'
      write(2,'(a)')  'EOF'
      close (2)

!
!  WARNING: system is a SUNOS special command not generally available
!  in FORTRAN.  Invoke plotxy - add plot command.
!
!  For ABSOFT compiler, use the /Applications/Absoft/lib/libU77.a library
!  which is the Unix library.
!

      call system('sleep 1')
      call system('(csh '//splot//')')

      if (present(output)) then
         if (len_trim(output) == 0) then
            write(output2,'(a)') 'newplot3.eps'
         else
            output2 = output
         endif
         call system('mv gnu3_myplot.eps '//trim(output2)//' ')
         call system('gv '//trim(output2)//'&') 
      else
         call system('mv gnu3_myplot.eps newplot3.eps')
         call system('gv newplot3.eps &')
      endif

!      call system('rm cgnu_data*.dat cgnu.exe')
      
      return

   end subroutine gnuplot3

!--------------------------------------------------------------------
! Additional Routines
!======================================================================

subroutine getlims(var, values, nfound)
      
!$$$$ calls ljust

!  Extracts an array of numbers from input in the plot_variables module.  
!  It is a large array in the plot_variables module at the end of the 
!  file, which has been filled earlier.

!  var     variable with the numbers to look for
!  values  the real output array of values found.
!  nfound  the number of numbers actually found in the input.
!          If the line contains fewer than nwant  values, this is the
!          value returned in  nfound.  If an error is discovered
!          nfound=-n, where  n  is the number of numbers properly
!          decoded.  If there are no numbers after the codeword
!          nfound=0.  

!********************************************************************

   implicit none

   integer, parameter :: nwant=2

   real(4), dimension(2), intent(out) :: values
   integer,               intent(out) :: nfound

   character (len=*) ::  var

   integer  :: l, ios, lbl, n, n1, n2,n3, lin
   character (len=80) ::  line,local,char

!********************************************************************

!  Initialize variables   
   values = 0.
   nfound = 0

!  If a comma is present, read after that
 
   line = adjustl(var)
 
   n3=index(line, ',')

   if (n3 > 0) then
      line = adjustl(line(n3+1:80))
   endif

!  If there is a comment %, avoid the rest after % 
   n2=index(line, '%')
   n2=80 + min(n2,1)*(n2 - 81)
   char=line(1:n2)

!  Find the two limits
   do n=1, nwant
      local =  adjustl(char)
      lbl=index(local, ' ')
      if (lbl .eq. 1) then
         nfound = n -1
         return
      endif
      read (local, *, iostat=ios) values(n)
      if (ios > 0) then
         print '(a)',' ',      &
         '>>> Unreadable numbers in this input line:',line
         nfound = l - n
         return
      endif
      char=local(lbl:80)
   enddo
   n=nwant+1
   nfound = n - 1  

   if (nwant /= nfound) then
      nfound = -99
   endif
          
   return

end subroutine getlims




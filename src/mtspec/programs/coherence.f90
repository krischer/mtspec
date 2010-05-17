module cohe_variables

!
! Module: define some global variables, that can be accesed and
! changed, modified by any function, program or subroutine
! that USES the specific module. The command is:
!   use module_name
!


   !
   !  For the scan subroutine
   !
            
   integer, parameter :: inmx = 500
   character (len=100), dimension(inmx) :: input
   character (len=100) :: output
   character (len=20)  :: type
   integer :: iecho, nin

   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !      Put here any global variables
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !

   real    ::              xbar, varx
   integer ::              nx,nf
   real    ::              dt 
   real, dimension(:), allocatable :: t
   real, dimension(:,:), allocatable :: x

   real(4) :: fact, tbp, p
   integer :: ntap, ntimes, kspec

   real(4), dimension(:), allocatable :: freq

   real(4), dimension(:), allocatable    :: cohe, phase, conf
   integer, dimension(:), allocatable    :: kopt
   real(4), dimension(:), allocatable    :: spec1, spec2
   real(4), dimension(:,:), allocatable  :: cohe_ci, phase_ci
   
   integer, parameter :: mxx=200000

                             
end module cohe_variables
                                                               

!----------------------------------------------------------------

program coherence

!
!  Multitaper Power Spectral Density code
!  See subroutine scan for synopsis of commands.
!
                    

!
!  Last modified 15 Sept 2005
!


!$$$$ calls getdat getone getint scan mtspec

!********************************************************************

   use cohe_variables
   use spectra
   use mvspectra
   use plot

   implicit none

   integer :: ignor, kwit, i
 
   character (len = 100) :: output2 


!********************************************************************

   !
   ! Initializing two of the variables of the module, 
   ! "Don't echo command lines when read"
   !
  
   iecho = -1
   nin   =  0

   !
   !  Read in the commands
   !

   do 
      call scan
      call getint('quit',ignor, kwit)

      if (kwit >= 0) then 
         dt = 0.0
         write(6,'(/a)') 'Normal Termination'
         stop
      endif

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Put here the call to your subroutines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

      if (allocated(x)) then
         deallocate(x)
      endif   
      if (allocated(t)) then
         deallocate(t)
      endif   

      call getdat

!  Spectral parameters

      call getint('ntap',ntap,kwit)
      if (kwit .ne. 1) then
         ntap = 0
      endif 

      call getint('ntimes',ntimes,kwit)
      if (kwit .ne. 1) then
         ntimes = 2 
      endif 

      call getone('fact',fact,kwit)
      if (kwit .ne. 1) then
         fact = 1.
      endif 

      call getone('tbnw',tbp,kwit)
      if (kwit .ne. 1) then
         tbp = 4.
      endif 

      call getint('kspec',kspec,kwit)
      if (kwit .ne. 1) then
         kspec = 7
      endif 
 
      call getone('conf',p,kwit)
      if (kwit .ne. 1) then
         p = 0.95
      endif 

      nf = nx/2 + 1

      if (allocated(freq)) then
         deallocate(freq)
      endif   
      if (allocated(spec1)) then
         deallocate(spec1)
      endif   
      if (allocated(spec2)) then
         deallocate(spec2)
      endif   
      if (allocated(cohe)) then
         deallocate(cohe)
      endif   
      if (allocated(phase)) then
         deallocate(phase)
      endif   
      if (allocated(kopt)) then
         deallocate(kopt)
      endif   
      if (allocated(conf)) then
         deallocate(conf)
      endif
      if (allocated(cohe_ci)) then
         deallocate(cohe_ci)
      endif
     if (allocated(phase_ci)) then
         deallocate(phase_ci)
      endif


      allocate(freq(nf))
      allocate(spec1(nf))
      allocate(spec2(nf))
      allocate(cohe(nf))
      allocate(phase(nf))
      allocate(kopt(nf))
      allocate(conf(nf))
      allocate(cohe_ci(nf,2))
      allocate(phase_ci(nf,2))

      kopt = kspec	! Set all freq to same # of tapers 

      call getchr('method',type,kwit)
      if (kwit>0) then
         if (0 .eq. index('sine para ',type(1:4))) then
            call mt_cohe (nx,dt,x(:,1),x(:,2),tbp,kspec,nf,p,   &
                 freq,cohe,phase,spec1, spec2,conf,cohe_ci, phase_ci ) 
         else
           call sine_cohe  (nx,dt,x(:,1),x(:,2),ntap,ntimes,fact,nf,p,	&
		freq,cohe,phase,spec1,spec2,kopt,conf,cohe_ci,phase_ci)
         endif
      else	! Standard is Thomson method
            call mt_cohe (nx,dt,x(:,1),x(:,2),tbp,kspec,nf,p,   &
                 freq,cohe,phase,spec1, spec2,conf,cohe_ci, phase_ci ) 
      endif

      call getchr('save',output,kwit)
      if (kwit>0) then
         output2 = trim(adjustl(output)) 
         open(12,file=output2,form='formatted')
         write(6,'(2a)')                      &
         'Output contains:  freq, spec1, spec2, cohe, phase, # tapers, ', &
         'conf, cohe_ci, phase_ci' 
         do i=1,nf
            write(12,'(5E16.7,i5,5E16.7)')             &
            freq(i), spec1(i), spec2(i), cohe(i), phase(i), kopt(i), &
            conf(i), cohe_ci(i,1), cohe_ci(i,2),phase_ci(i,1), phase_ci(i,2)
         enddo 
         close(12)
      endif


      call getint('plot',ignor, kwit)
      if (kwit>=0) then

         if (ignor == 1) then

            allocate(t(nx))
            t = real( (/(i-1, i=1,nx)/) )*dt

            call gnuplot(t,x(:,1),'plot',ylimit='4',xlimit='5',  &
			frame=' ')
            call gnuplot(t,x(:,2),ylimit='4',xlimit='5',         &
			frame='frame on top right -xaxis',output='ts.eps')
            deallocate(t)

         endif


         call gnuplot(freq,conf,'hold',color='2',xlabel='Frequency (Hz)', &
                    ylabel='Coherence')
         call gnuplot(freq,cohe,'plot',color='1',frame=' ',& 
                    ylimit='4 0. 1.',xlimit='5')
         call gnuplot(freq,phase,frame='frame on top right -xaxis',  &
                    ylimit='4 -200 200',xlimit='5',color='black',    &
                    ylabel='Phase',output='coh_plot.eps')
      endif

      write(6,'(/a)')'Enter further commands or "quit" to terminate run'
      
   enddo

end program coherence


!_______________________________________________________________

!
! Contains
!

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Put here your subroutines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

!_______________________________________________________________________


!=======================================================================
!        Unit K:    Decoding routines for command kit
!=======================================================================

subroutine scan

!$$$$ calls clear getchr icheck

!  Input routine for commands

!  Reads from the standard input until eof or code "execute " or "quit".
!  Saves lines in the input in cohe_variables for later retrieval by
!  getarr, getone or getchr.

!  Prints a glossary upon request

!********************************************************************

   use cohe_variables

   implicit none
   
   integer :: n1, ignor, i, ios, l
   character *80 line, code*4

!********************************************************************


   if (nin .eq. 0) then 
      write(6,'(a)') ' ', &
      'Enter commands for coherence estimation (? for help)'
   endif
   
   do l=nin+1, inmx
      
      read(*,'(80a)', iostat = ios) line
      if (ios<0) then
         stop
      endif
      
      call ljust(line,line)         ! Remove leading blanks
      
      if (line(1:4) == 'exec') then
         exit
      endif
      
   !
   !  List a glossary of codes
   !
   
      if (line(1:1) .eq. '?') then
         write(6,'(a/(2x,a))') &
     'Enter commands from the following list:', &
     '?:                          Remind me of the command list again', &
     'execute:          With given parameters start coherence program', &
     'file file1 [file2]:       Enter filename(s) of data (mandatory)', &
     'method type:    Enter the method to use (Thomson (def) - Sine )', &
     'interval dt:                          Sampling interval of data', &
     'nterms n:                      Number of data points to be read', &
     'skip k:                   Skip over k lines before reading data', &
     'column k n:         Read data from columns k [and n] in a table', &
     'kspec k:       Number of taper for Slepian tapers (Default = 7)', &
     'tbnw nw:     Time-bandwidth product for Thomson method (def=4.)', &
     'ntap ntap:        Number of tapers to be used (constant number)', &
     'ntimes ntimes:  # Iterations adaptive spectrum (overrides ntap)', &
     'fact factor:    The spectral smoothing for derivative estimates', &
     'conf p:     Probability that the coherence exceeds 0 (def 0.95)', &
     'save file:             The file to save the coherence spectrum ', &
     'review:                           Display current command stack', &
     'clear command:     Delete most recent occurrence of the command', &
     'plot i:         Make plots of cohe/phase (to plot data set i=1)', &
     ' '

      !
      !  Review the command stack
      !

      elseif (line(1:4) .eq. 'revi') then
          write(6,'(5x,a)')' ', '=================== ',     &
          (input(i)(1:60),i=1,nin),'=================== '

      elseif (line(1:1) .ne. ' ') then
!  Translate homonyms
         if (line(1:4) .eq. 'echo') then
            iecho=-iecho
         endif

         nin=nin + 1
         if (nin > inmx) then
            write(6,'(a)') '>>>> Too many commands - memory full'
            stop
         endif
         call icheck(line)
         input(nin)=line
         if (line(1:4) .eq. 'quit') then 
            return
         endif

         !
         !  Clear a command and clear clear itself
         !

         if (line(1:4) .eq. 'clea') then
            call getchr('clear', code, ignor)
            call clear (code)
            nin=nin - 1
         endif
      endif
      
   enddo 

   n1=max(1, nin-24)
   write(6,'(5x,a)')' ', '=================== ',      &
   (input(i)(1:60),i=n1,nin),'=================== '

return

end subroutine scan

!______________________________________________________________________


subroutine icheck(line)
      
!$$$$ calls nothing

!  Checks the command fragment com against the catalog; appends a
!  warning if com is not present in the list.

!********************************************************************

   use cohe_variables

   implicit none

   character*80 line, com*4

!********************************************************************


   com=line(1:4)
   if (0 .eq.  &
      index('clea colu detr file inte ',com)  &
      +index('nter outp skip save ',com)   &
      +index('ntap ntim fact quit ', com)  &
      +index('kspe tbnw meth conf plot ', com)) then
      line=line(1:20)//' %<<<<<<< Unrecognized command'
   endif

   return

end subroutine icheck

!______________________________________________________________________

subroutine getarr(code, values, nwant, nfound)
      
!$$$$ calls ljust

!  Extracts an array of numbers from input in the cohe_variables module.  
!  It is a large array in the cohe_variables module at the end of the 
!  file, which has been filled earlier.

!  code    A 4-byte identifying code.  Only lines in the input store
!          beginning with this code are scanned for numbers.
!  values  the real output array of values found.
!  nwant   the maximum number of numbers expected .
!  nfound  the number of numbers actually found in the input.
!          If the line contains fewer than nwant  values, this is the
!          value returned in  nfound.  If an error is discovered
!          nfound=-n, where  n  is the number of numbers properly
!          decoded.  If there are no numbers after the codeword
!          nfound=0.  Finally, if the code is absent from the store
!          nfound=-99 and the array is left undisturbed.

!********************************************************************

   use cohe_variables

   implicit none

   integer  :: l, ios, lbl, n, n1, n2, lin, nwant, nfound
   real     :: values
   character *80 line,local,char, code*4
   dimension values(*)

!********************************************************************
   
   
   !
   !  Read the store in reverse order (Thus last entry is obeyed)
   !
   
   do lin=nin, 1, -1
      line=input(lin)
      
      !
      !  Check for code 
      !
      
      if (code == line(1:4)) then 
         
         if (iecho .ge. 1) then
            write(6,'(2a)')'==> ',line
         endif
         
         n1=index(line, ' ')+1
         n2=index(line, '%')
         n2=80 + min(n2,1)*(n2 - 81)
         char=line(n1:n2)

         do n=1, nwant
            call ljust(char, local)
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
         return
      endif
          
   enddo
   
   !
   !  Code word not found
   !
   
   nfound=-99
   
return

end subroutine getarr

!______________________________________________________________

subroutine getone(code, value, nfound)

!$$$$ calls getarr

!  Extracts a single number from the input store.  That store is
!  a large array in the cohe_variables module at the end of the 
!  file, which has been filled earlier.

!  code    A 4-bye identifying code.  Only lines in the input store
!          beginning with this code are scanned for numbers.
!  value   the real output variable containing the desired number.
!  nfound  is 1 if a number is successfully read in; it is zero
!          the number is absent or unreadable.  nfound = -99 if the
!          code is absent from the input store.

!********************************************************************

   use cohe_variables

   implicit none

   character*4 code
   real, dimension(1) :: v
   real               :: value
   integer            :: nfound

!********************************************************************


   call getarr(code, v, 1, nfound)
   
   if (nfound .eq. 1) then 
      value=v(1)
   endif

return

end subroutine getone

!______________________________________________________________

subroutine getint(code, number, nfound)

!$$$$ calls getarr


!  Extracts a single integer from the input store.  
!  See getone for details.
!

!********************************************************************

   use cohe_variables

   implicit none

   character*4 code
   real, dimension(1)   :: v
   integer              :: number, nfound

!********************************************************************


   call getarr(code, v, 1, nfound)
   if (nfound .eq. 1) then
      number=nint(v(1))
      
      if (number .ne. v(1)) then
         write(6,'(4a,1p,g16.8/a,i10)')        &
         '>>> Warning: ',code,' expects an integer argument, but', &
         ' found: ',v(1),'    Returns: ',number,' '
      endif
      
   endif
      
   return
      
end subroutine getint
      
!______________________________________________________________
 
subroutine getchr(code, char, nbytes)

!      $$$$ calls ljust

!  Extracts a single character variable from the input store.  That 
!  store is a large array in the cohe_variables module at the end
!  of the file, which has been filled earlier.
!  

!  code    A 4-byte identifying code.  Only lines in the input store
!          beginning with this code are scanned.
!  char    the character output variable containing the desired string.
!  nbytes  is the length of the string read; it is zero if
!          the line is blank after the code.  nbytes = -99 if the
!          code is absent from the input store.
!

!********************************************************************

   use cohe_variables

   implicit none

   integer :: k, nn, n1, n2, lin, nbytes
   character *80 line, char*(*),  code*4

!********************************************************************

   
   !
   !  Inspect the store in reverse order (thus reading latest entry)
   !
   
   do lin=nin, 1, -1
      line=input(lin)
        
      !
      !  Check for code word
      !
          
      if (code == line(1:4)) then
         
         if (iecho >= 1) then
            write(6,'(2a)')'==> ',line
         endif   
              
         n1=index(line, ' ')+1
         n2=index(line, '%')
         n2=80 + min(n2,1)*(n2 - 81)

         !
         !  Check for blank string
         !
            
         nbytes=0
         if (line(n1:n2) == ' ') then
            return
         endif
                    
         !  
         !  Save in char everything from 1st non-blank to last non-blank
         !
                
         call ljust(line(n1:n2), char)
         nn=min(n2-n1+1, len(char))
              
         do k=nn, 1, -1
            if (char(k:k) .ne. ' ') then
               exit
            endif
         enddo
                                                                             
         nbytes= k
         return
         
      endif      
                                                                          
   enddo

   !
   !  Code word not found
   !
 
   nbytes=-99
   return

      
end subroutine getchr


!______________________________________________________________
      
subroutine clear(code)
      
!$$$$ calls nothing

!
!  Removes the last command entry identified by code.
!  code    A 4-byte identifying code.  Only lines in the input store
!          beginning with this code are scanned.
!

!********************************************************************

   use cohe_variables

   implicit none

   integer :: lin
   character *80 line, code*4

!********************************************************************

         
   !
   !  Inspect the store in normal order
   !
   
   do lin= nin, 1, -1
      
      line=input(lin)

      !
      !  Check code  and clear the line if present, but only once
      !
      
      if (code .eq. line(1:4)) then
         input(lin)=' --'
         return
      endif
      
   enddo
   
return
      
end subroutine clear
      
!______________________________________________________________
      
subroutine ljust(str1, str2)
      
!$$$$ calls nothing

!  Left justify: input character string str1 (up to 80 bytes in length)
!  is left justified, removing leading blanks, and returned in str2.
!  str1 and str2 may be set to the same variable in the call.

!********************************************************************

   use cohe_variables

   implicit none

   integer :: j, l1
   character*(*) str1, str2, str3*80

!********************************************************************


   str2=str1
   
   if (str1 == ' ') then
      return
   endif
      
   l1=len(str1)
   str3=str1
      
   do j=1, l1
   
      if (str1(j:j) .ne. ' ') then
         str2=str3(j:l1)
         return
      endif
           
   enddo
   
   return

end subroutine ljust

   
!______________________________________________________________________

function later(code1, code2)
      
!$$$$ calls nothing

!  Returns the difference in order number in the stack of the
!  most recent occurrence of the commands code1, code2.  If the
!  command isn't present assigns it the order number zero.
!  Thus later > 0 if code1 occurs after code 2, < 0 if before,
!  and later=0 if both are absent and code1 and code2 are different.

!********************************************************************

   use cohe_variables

   implicit none

   integer :: lin, kode2, kode1, later
   character *80 line, code1*4,code2*4

!********************************************************************

   !
   !  Inspect the store
   !
   
   kode1=0
   kode2=0
   
   do lin=1, nin
   
      line=input(lin)
      if (code1 .eq. line(1:4)) then
         kode1=lin
      endif
      
      if (code2 .eq. line(1:4)) then
         kode2=lin
      endif

   enddo

   later=kode1 - kode2
      
   return

end function later


!=======================================================================
!                Unit D:  Get the Data
!=======================================================================

subroutine getdat

!$$$$ calls getarr getchr getone getint

!
!  Gets the file name and reads data into vector x().
!  I define a y() vector, just in this program, to be 
!  able to allocate the x() vector with the size of the 
!  data series.
!
!  Issues error messages.
!

!********************************************************************

   use cohe_variables

   implicit none
   
   character (len=64)      :: name, temp
   character (len=64), dimension(2) ::  name1
   real, dimension(40)     :: tab
   real, dimension(mxx,2)    :: y
   real                    :: dum
   real, dimension(2)      :: column, rskip
   integer :: l, koln, kols, intdt, nte, j, nt, none
   integer :: nterm, ios, itwas, koln2

   integer :: kbl, nfiles, i1, i2, nfl, nskip
   integer, dimension(2) :: iskip

   data nterm/mxx/
   save nterm

!********************************************************************

     
   name(1:1) = '  '  ! Assign a default name.
   column = 1        ! If no column assigned, use column = 1
   iskip  = 0	     ! If no skipping assigned, do not skip
 
   call getchr('file', name, itwas)
   if (itwas == 0) then
      write(6,'(a)')'>>> Psd cannot continue without a file name'
      stop
   endif
   if (itwas == -99) then
      write(6,'(a)')'>>> Command file is mandatory'
      stop
   endif
   kbl = index(name(1:itwas), ' ')
   nfiles = min(2,kbl+1)
   if (kbl == 0) then
      kbl = itwas 
   endif

!
!  If there are two files, squeeze out excess blanks in name

   if (nfiles .eq. 2)  then
      call ljust(name(kbl:itwas), temp)
      name(kbl+1:itwas)=temp
      write(6,'(a)') 'Data will be read from two separate files:',name
      column(2)=1
   endif
   i1=1
   i2=kbl

   !
   !  Number of terms to skip
   !

   nskip = 0
   call getarr('skip', rskip, 2, nskip)
   
   if (nskip > 1) then 
      iskip(1)  = rskip(1)
      iskip(2)  = rskip(2)
   elseif (nskip == 1) then
      iskip  = rskip(1)
   else
      iskip = 0
   endif

   if (nfiles == 2 ) then
      name1(1) = name(i1:i2)
      name1(2) = name(i2+1:itwas)
   else
      name1(1) = name
   endif 

   !
   !  Skip records before reading
   !
 
   do nfl = 1,nfiles
      open(11,file=name1(nfl), status='OLD', iostat=ios)
      if( ios/=0) then
         write (6,'(2a)') 'Unable to open file: ', name1(nfl) 
         stop
      endif
 
      if (none >= 1 .and. iskip(nfl) > 0) then
         do j=1,  iskip(nfl)
            read (11,*, iostat = ios) dum
            if (ios<0) then
               write(6,'(2a)') '>>> End of file encountered', &
                                ' while skipping'
               stop
            endif
         enddo
         write(6,'(a,i7)')'   Skipped records before data:',iskip(nfl)
      endif

   !
   !  Get number of terms to be read
   !
   
      nt=mxx
      call getint('nterms', nterm, nte)
      if (nte > 0) then
         nt=min(mxx, nterm)
      endif
   
   !
   !  Get the sampling interval
   !
   
      call getone('interval', dt, intdt)
      if (intdt < 0) then
         dt = 1.0
      endif


   !
   !  Get column(s) to be read
   !

      call getarr('column', column, 2, kols)
   
      if (kols > 1) then 
         koln  = column(1)
         koln2 = column(2)
      elseif (kols == 1) then
         koln  = column(1)
         koln2 = koln
      else
         write(6,'(a)') 'Column has to be either 1 or two values'
         stop
      endif

   !
   !  Read from two columns in same file, given by user
   !


      if (nfiles == 1) then
         write(6,'(a,2i4)')'Series read from columns ',koln,koln2
         do j=1, nt
            read (11,*, iostat = ios) (tab(l),l=1,max(koln,koln2))
            if (ios > 0) then
               write(6,'(2a,i7)') '>>> Unreadable number in ', &
                                  'data file at point ', j
               stop
            endif
            if (ios<0) then
               nx = j-1
               write(6,'(a)') '          EOF detected in data file'
               rewind(unit=11)
               write(6,'(a,i7)')'       Number of terms read:',nx
               allocate(x(nx,2))
               x(:,1)  = y(1:nx,1)
               x(:,2)  = y(1:nx,2)
               exit
            endif
            y(j,1) = tab(koln)
            y(j,2) = tab(koln2)
         enddo
      
   !   
   !  Read from 2 files. 
   !
      
      else

         koln = nint(column(nfl))
         write(6,'(2(a,i3))')'Reading file number',nfl,',  column ',koln

         do j = 1,nt
            read (11, *, iostat=ios) (tab(l),l=1, koln)
            if (ios>0) then 
               write(6,'(2a,i7)')'>>> Unreadable number in data', &
                                'file at point',j
               stop
            elseif(ios<0) then 
               nx = j-1
               write(6,'(a)') '          EOF detected in data file'
               rewind(unit=11)
               write(6,'(a,i7)')'         Number of terms read:',nx
               if (allocated(x)) then
                  write(6,'(a,i7)')' Already allocated array:', nx
               else
                  allocate(x(nx,2))
               endif
               x(:,nfl)  = y(1:nx,nfl)
               exit
            endif
            y(j,nfl) = tab(koln)
         enddo
      endif
   enddo
      
   if (ios >= 0) then

      if (nt == mxx) then 
         write(6,'(2a,i7,a)')        & 
         '>>> Array space filled:',  &
         ' series truncated to',mxx,' terms'
         nx=nt
         allocate(x(nx,2))
         x(:,1)  = y(1:nx,1)
         x(:,2)  = y(1:nx,2)
      else
         nx = nt
         allocate(x(nx,2))
         x(:,1)  = y(1:nx,1)
         x(:,2)  = y(1:nx,2) 
      endif
   
   endif

   write(6,'(a,i7)')'          Number of terms read:',nx

   if (nx == 0) then
      allocate(x(mxx,2))    ! If nx is zero, need to allocate x()
   endif
  
   close(11) 

return
      
end subroutine getdat


!___________________________________________________________________

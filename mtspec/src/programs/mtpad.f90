module pad_variables

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
   character (len=80), dimension(inmx) :: input
   character (len=100) :: output
   character (len=20)  :: type, pltype
   integer :: iecho, nin

   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !      Put here any global variables
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !

   real    ::              xbar, varx, smin, smax, ebar 
   integer ::              nx,nf, nfft
   real    ::              dt 
   real, dimension(:), allocatable :: x, t

   real, dimension(:),    allocatable      :: freq, spec, fstat
   real, dimension(:,:),  allocatable      :: err
   real, dimension(:),    allocatable      :: freq2, err2
   integer, dimension(:), allocatable      :: kopt


   real(4) :: tbp, fact, fcrit
   integer :: ntap, ntimes, kspec, rshape

   integer, parameter :: mxx=2000000

   character (len=40) :: xlimit,ylimit

                             
end module pad_variables
                                                               

!----------------------------------------------------------------

program mtpsd 

!
!  Multitaper Power Spectral Density code
!  See subroutine scan for synopsis of commands.
!
                    

!
!  Last modified 15 Sept 2005
!


!$$$$ calls getdat getone getint scan mtspec

!********************************************************************

   use pad_variables
   use spectra
   use plot

   implicit none

   integer :: ignor, kwit, j, i, qi, itwas

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

      call getdat

      if (allocated(t)) deallocate(t)
      allocate(t(nx))

      t = real( (/(i-1, i=1,nx)/) )*dt

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
         kspec = (2*tbp - 1)
      endif 

      call getint('nfft',nfft,kwit)
      if (kwit .ne. 1) then
         nfft = nx * 2
      endif 
      if (nfft < nx) then
         nfft = nx * 2
      endif

      nf = nfft/2 + 1

      if (allocated(freq)) deallocate(freq)
      if (allocated(spec)) deallocate(spec)
      if (allocated(kopt)) deallocate(kopt)
      if (allocated(err))  deallocate(err)
      if (allocated(fstat))  deallocate(fstat)

      allocate(freq(nf))
      allocate(spec(nf))
      allocate(kopt(nf))
      allocate(err(nf,2))
      allocate(fstat(nf))

      call getint('line',rshape,kwit)
      if (kwit .ne. 1 .or. rshape>2) then
         rshape = -1
      endif 

      call getone('fval',fcrit,kwit)
      if (kwit .ne. 1) then
         fcrit = 0.99
      endif 

      call getchr('method',type,kwit)
      if (kwit>0) then

         if (0 .eq. index('quadratic qispec ',type(1:4))) then
            qi = 0
         else
            qi = 1	! Use the multitaper QI theory
         endif

         if (0 .eq. index('sine para ',type(1:4))) then
            if (rshape>=0) then 
               call mtspec(nx,nfft,dt,x,tbp,kspec,nf,freq,spec,err=err,    &
                           rshape=rshape,fcrit=fcrit,fstat=fstat,          &
                           verb='y',qispec=qi)
            else
               call mtspec(nx,nfft,dt,x,tbp,kspec,nf,freq,spec,   &
                           err=err,qispec=qi)
            endif
         else
           call sine_psd(nx,dt,x,ntap,ntimes,fact,nf,freq,spec,kopt,err)
         endif

      else	! Standard is Thomson method

         if (rshape>=0) then 
            call mtspec(nx,nfft,dt,x,tbp,kspec,nf,freq,spec,err=err,    &
                        rshape=rshape,fcrit=fcrit,fstat=fstat,          &
                        verb='y',qispec=qi)
         else
            call mtspec(nx,nfft,dt,x,tbp,kspec,nf,freq,spec,err=err,qispec=qi)
         endif
      endif

!  Check whether to plot or not

      call getint('plot',ignor, kwit)
      if (kwit>=0) then

! Some plotting parameters

         call getchr('output', output, itwas) ! Output of figure
         if (itwas<=0) then
            output='spec.ps'
         endif
         call getchr('xlimit', xlimit, itwas)
         if (itwas<=0) then
            xlimit='6'
         else
            xlimit='6 ' // trim(adjustl(xlimit)) 
         endif
         call getchr('ylimit', ylimit, itwas)
         if (itwas<=0) then
            ylimit='5'
         else
            ylimit='5 ' // trim(adjustl(ylimit)) 
         endif

!  Plot data series

         if (ignor == 1) then
            call gplot(t,x,output='ts.ps')
         endif

!  Fill the confidence intervals

         call getint('fill',kwit,kwit)
         if (kwit > -90) then
            if (allocated(freq2)) deallocate(freq2) 
            if (allocated(err2))  deallocate(err2)
            allocate(freq2(2*nf-2))
            allocate(err2(2*nf-2))
            freq2(1:nf-1) = freq(2:nf)
            err2(1:nf-1)  = err(2:nf,1)
            do i = 1,nf-1
               freq2(nf-1+i) = freq(nf-i+1)
               err2(nf-1+i)  = err(nf-i,2)
            enddo
         
            call gplot(freq2,err2,'hold',fill='fill',logxy='loglog')      
!         else
!            call gplot(freq,err,'hold',logxy='loglog')
         endif

         call getchr('logxy',pltype,kwit)
         if (kwit>0) then
            if (0 < index('linlog',pltype(1:6))) then
               print *, 'Doing linlog'
               call gplot(freq,spec,logxy='linlog',output=output,   &
			xlimit=xlimit,ylimit=ylimit)
            elseif (0 < index('linlin',pltype(1:6))) then
               call gplot(freq,spec,logxy='linlin',output=output,   &
   			xlimit=xlimit,ylimit=ylimit)
            elseif (0 < index('loglin',pltype(1:6))) then
               call gplot(freq(2:nf),spec(2:nf),logxy='loglin',     &
			output=output,xlimit=xlimit,ylimit=ylimit)
            else
               call gplot(freq(2:nf),spec(2:nf),logxy='loglog',     &
			output=output,xlimit=xlimit,ylimit=ylimit)
            endif
         else	
            call gplot(freq,spec,logxy='loglog',output=output,   &
			xlimit=xlimit,ylimit=ylimit)
         endif

         if (rshape>=0) then 
            call gplot(freq,fstat,logxy='linlin',output='flinear.ps')
         endif

      endif

!  Output to be saved

      call getchr('save',output,kwit)
      if (kwit>=0) then
         open(12,file=output)
         write(12,'(5g13.5)')		&
		 (freq(j),spec(j),err(j,1),err(j,2),fstat(j),j=1,nf)
         close(12)
      endif


      write(6,'(/a)')'Enter further commands or "quit" to terminate run'
      
   enddo

end program mtpsd


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
!  Saves lines in the input in pad_variables for later retrieval by
!  getarr, getone or getchr.

!  Prints a glossary upon request

!********************************************************************

   use pad_variables

   implicit none
   
   integer :: n1, ignor, i, ios, l
   character *80 line, code*4

!********************************************************************


   if (nin .eq. 0) then 
      write(6,'(a)') ' ', &
      'Enter commands for spectral estimation (? for help)'
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
     'execute:          With given parameters start ???? program ????', &
     'file filename:        Enter filename of data series (mandatory)', &
     'method type:      Method to use (Thomson (def) - Sine - qispec)', &
     'logxy type:        Enter the type, linlin, loglog (def), linlog', &
     'interval dt:                          Sampling interval of data', &
     'nterms n:                      Number of data points to be read', &
     'nfft n:          Number of fft points to pad data to (2*nterms)', &
     'skip k:                   Skip over k lines before reading data', &
     'column n:                    Read data from column k in a table', &
     'kspec ntapers:                      Number of tapers to be used', &
     'tbnw nw:                             The time-bandwidth product', &
     'fill:                             The errors shown in gray area', &
     'ntap ntap:        Number of tapers to be used (constant number)', &
     'ntimes ntimes:  # Iterations adaptive spectrum (overrides ntap)', &
     'fact factor:    The spectral smoothing for derivative estimates', &
     'save file:		  The file to save the output spectra', &
     'output file:           filename to output figure (def spec.ps) ', &
     'line rshape    0-reshape line 1-remove line 2-only around 60 Hz', &
     'fval prob       Probability threshold in F statitics for a line', &
     'review:                           Display current command stack', &
     '(x,y)limit x1 x2 [dx]:            limits. No xlength supported.', &
     'plot i:            Make plots of spectra (to plot data set i=1)', &
     'clear command:     Delete most recent occurrence of the command', &
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

   use pad_variables

   implicit none

   character*80 line, com*4

!********************************************************************


   com=line(1:4)
   if (0 .eq.  &
      index('clea colu detr file inte ',com)  &
      +index('nter outp skip save meth ',com)   &
      +index('ntap ntim fact type logx',com)   &
      +index('line fcrit xlim ylim fval',com)   &
      +index('kspec tbnw quit fill nfft line plot', com)) then
      line=line(1:20)//' %<<<<<<< Unrecognized command'
   endif

   return

end subroutine icheck

!______________________________________________________________________

subroutine getarr(code, values, nwant, nfound)
      
!$$$$ calls ljust

!  Extracts an array of numbers from input in the pad_variables module.  
!  It is a large array in the pad_variables module at the end of the 
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

   use pad_variables

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
!  a large array in the pad_variables module at the end of the 
!  file, which has been filled earlier.

!  code    A 4-bye identifying code.  Only lines in the input store
!          beginning with this code are scanned for numbers.
!  value   the real output variable containing the desired number.
!  nfound  is 1 if a number is successfully read in; it is zero
!          the number is absent or unreadable.  nfound = -99 if the
!          code is absent from the input store.

!********************************************************************

   use pad_variables

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

   use pad_variables

   implicit none

   character*4 code
   real, dimension(1)   :: v
   integer              :: number, nfound

!********************************************************************


   call getarr(code, v, 1, nfound)
   if (nfound .eq. 1) then
      number=nint(v(1))
      
      if (number .ne. v(1)) then
         print '(4a,1p,g16.8/a,i10)',        &
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
!  store is a large array in the pad_variables module at the end
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

   use pad_variables

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

   use pad_variables

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

   use pad_variables

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

   use pad_variables

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

   use pad_variables

   implicit none
   
   character (len=64)      :: name
   real, dimension(40)     :: tab
   real, dimension(mxx)    :: y
   real                    :: dum
   real                    :: column(1)
   integer :: l, koln, kols, intdt, nte, j, nt, iskip, none
   integer :: nterm, ios, itwas
   data nterm/mxx/
   save nterm

!********************************************************************

     
   name(1:1) = '  '  ! Assign a default name.
   column = 1        ! If no column assigned, use column = 1
  
   call getchr('file', name, itwas)
   if (itwas == 0) then
      write(6,'(a)')'>>> Psd cannot continue without a file name'
      stop
   endif
   if (itwas == -99) then
      write(6,'(a)')'>>> Command file is mandatory'
      stop
   endif

      
   !  Continue reading from the same file if name='-continue'
   !  otherwise close previous unit, reopen new file

   if (name(1:4) /= '-con')  then
      close(unit=11)
      open (unit=11, file=name, status='OLD', iostat=ios)
      if( ios/=0) then
         write (6,'(2a)') 'Unable to open file: ', name 
         stop
      endif
   endif

   !
   !  Skip records before reading
   !
   
   call getint('skip', iskip, none)
   if (none >= 1 .and. iskip > 0) then
      do j=1,  iskip
         read (11,*, iostat = ios) dum
         if (ios<0) then
            write(6,'(2a)') '>>> End of file encountered', &
                             ' while skipping'
            stop
         endif
      enddo
      write(6,'(a,i7)')'   Skipped records before data:',iskip
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

   call getarr('column', column, 1, kols)
    
   if (kols > 1) then 
      write(6,'(2a)') &
      '>>> Warning: column should take only ',   &
      '1 number - Using the first term'
   endif

   !
   !  Read from a single column, given by user
   !


   if (column(1) >= 1) then
      koln=column(1)
      do j=1, nt
         read (11,*, iostat = ios) (tab(l),l=1,koln)
         if (ios > 0) then
            write(6,'(2a,i7)') '>>> Unreadable number in ', &
                               'data file at point ', j
            stop
         endif
         if (ios<0) then
            nx = j-1
            write(6,'(a)') '          EOF detected in data file'
            rewind(unit=11)
            write(6,'(a,i7)')'         Number of terms read:',nx
            allocate(x(nx))
            x = y(1:nx)
            return
         endif
         y(j)=tab(koln)
      enddo
      
   !   
   !  Read every number on each line, the matrix.
   !
      
   elseif (column(1) == 0) then
      !  Fill y with junk
      do j=1, nt
         y(j)=-0.4342944e+15
      enddo
      read (11, *, iostat=ios) (y(j),j=1, nt)
      if (ios>=0) then 
         write(6,'(2a,i7)')'>>> Unreadable number in data', &
                          'file at point',j
         stop
      elseif(ios<0) then 
         do j=1, nt
            if (y(j) == -0.4342944e+15) then
               nx = j-1
               write(6,'(a)') '          EOF detected in data file'
               rewind(unit=11)
               write(6,'(a,i7)')'         Number of terms read:',nx
               allocate(x(nx))
               x = y(1:nx)
               return
            endif
         enddo
      endif
   endif
      
   if (nt == mxx) then 
      write(6,'(2a,i7,a)')        & 
      '>>> Array space filled:',  &
      ' series truncated to',mxx,' terms'
      nx=nt
      allocate(x(nx))
      x = y(1:nx)
   else
      nx = nt
      allocate(x(nx))
      x = y(1:nx)
   endif
   
   write(6,'(a,i7)')'          Number of terms read:',nx

   if (nx == 0) then
      allocate(x(mxx))    ! If nx is zero, need to allocate x()
   endif
   

return
      
end subroutine getdat





!___________________________________________________________________

                       program plotxy
c  General-purpose plotting program written and documented by
c  Bob Parker and Loren Shure with routines from Alan Chave
c
c             Conforms to ANSI FORTRAN77 standard
c
c **********************************************************************
c  For notes on conversion to different computers see routine  ragbag
c **********************************************************************
c
c$$$$ calls ask minmax pltsub ragbag readin
c
c  Main program for  plotxy,  a minimum-fuss plot package
c  allowing data from disk files to be plotted under control of a
c  simple command language.  All data to be plotted are stored in
c  memory as separate series
c
      character*4 code,comand(50),linlog(2)*6
      character*116  line,iylab,ixlab,ifmt,itit,image
      character*64 name,pfile,note*80,tnote*80,morsel*16
c
      parameter (maxxy=75 000, maxnot=51, maxsel=600, maxknt=500)
c
      common // xrange(2),x(maxxy),yrange(2),y(maxxy)
      common /char1/ code,line,image
      common /char2/ pfile /nchr2/ npfile
      common /char3/ name,ifmt /nchr3/ nname
      common /char4/ ixlab,iylab,itit
      common /char5/ note(maxnot),tnote,morsel(maxsel)
      common /xyaxes/ nux,nuy,nax,nay,right,top,ixe,iye
      common /grdlog/ logik,xslg,ngrx,ngry,grxy(200)
      common /backs/ backy,ht,hgt,htix
      common /inform/ nchar,nfound,nerr,xdata(10)
      common /table/ kount(20,maxknt),ncount,ioff
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /paramr/ mode,x0,delx,ifopen,ifile,dash(2),laffin,affxy(4)
      common /inout/ in,iout,idisk
      common /paramp/ xlim(4),ylim(4),nxlab,nylab,ntit,hxlab,hylab,htit,
     $   height,angle,lw,iframe,kolab(3)
      common /notes/ nnotes,xyh(9,maxnot),leng(maxnot),kolnot(maxnot),
     $   jot1,xynote(2,maxsel),kolmrs(maxsel),hmrs(maxsel)
c
      common /penops/ iup,idn,mvor
      common /kink/ fill(2),xo,yo,snext,ink
      common /post/ boxx,boxy,land,infill
      common /lnprop/ kolor,linfat
c
      data comand/  'help','stop','file','form','mode','smoo','symb',
     $'read','canc','logx','dash','affi','skip','colo','weig','fill',
     $'pale','back',  2*'    ',
     $'xlim','ylim','xlab','ylab','titl','outp','land','char','plot',
     $'offs','fram','note',16*'    ',  'save','stat'/
      data  linlog/'Linear','Log   '/, nerror/0/,ngraph/0/
c
c  Initialize a few things
      call ragbag(0, 0)
c
      height=0.11
      call letter(0.0,0.0, height, 0.0, char(92)//'comp')
c
 900  ngraph=ngraph + 1
      write(*,*) ' Enter commands for graph',ngraph
c  Decode command.  If it's in list perform instruction below
 1000 call ask
      do 1100 list=1,50
        if (code .eq. comand(list)) goto 1200
 1100 continue
      write(*,'(1x,a/a)') code,
     $' >>>> This command not recognized'
      nerror=nerror + 1
      if (nerror.lt. 20) goto 1000
      write(*,*)'>>>> Too many errors - plotxy quits'
      goto 6000
c
c  Performs commands
 1200 if (list.eq.1)  goto 2000
      if (list.eq.2)  goto 6000
      if (list.eq.49) goto 4000
      if (list.eq.50) goto 3000
      if (list.le.20) call readin(list)
      if (list.gt.20) call pltsub(list-20)
      if (list.eq.29) goto 900
      goto 1000
c
c ------ command  help ------
 2000 write(*,'(a/(6a5))')'Available commands:',(comand(j),j=1,50)
      write(*,'(a)')' ','        Unix systems only',
     $'If on-line  documentation is installed and you want details ',
     $'about a particular command (say mode), type in another window:',
     $'doc plotxy mode     and more information will be provided.'
      goto 1000
c
c ----- command  status ------
c   Print out status of parameters
 3000 write(*, '(/(a,t46,i6))')
     $' Date of this version: 2/11/',2007,
     $' Max number of points in all series (MAXXY)',maxxy,
     $' Max number of data series (MAXKNT)',maxknt,
     $' Max number of full notes (MAXNOT)',maxnot-1,
     $' Max number of short notes (MAXSEL)',maxsel
      write(*,300)
     $name(1:nname),pfile(1:npfile),ifmt(1:58),ifmt(59:116)
 300  format(/' Current read and plot file names are '/1x,a /1x,a /
     $' Read format ',a58/13x,a58)
      if (abs(mode).ne. 1) write(*,301) mode,' '
      if (abs(mode).eq. 1) write(*,301) mode,'x0 =',x0,'delx =',delx
 301  format(' Read mode ',i2,2(3x,a,g12.4))
      if (dash(1).ne.0.0) write(*,302) dash
 302  format(' Current dash constants  ', 2f6.2)
      if (laffin.ne.0) write(*,305) affxy
 305  format(' Affine  transformation ', 4g12.4)
c  Tell axes attributes
      if (nxlab.gt.0) write(*,*) ' xlabel:',ixlab(1:nxlab)
      if (nylab.gt.0) write(*,*) ' ylabel:',iylab(1:nylab)
      if (ntit .gt.0) write(*,*) '  title:',itit (1:ntit )
      lx=1 + mod(logxy,2)
      ly=1 + logxy/2
      write(*,'(1x,2a,'' axis, length'',f6.2,a,g10.3,a,g10.3)')
     $linlog(lx),' x ',xlim(1),'  from ',xlim(2),' to',xlim(3),
     $linlog(ly),' y ',ylim(1),'  from ',ylim(2),' to',ylim(3)
      if (xlim(2).le.0.0 .and. lx.eq.2 .and. xlim(2).lt.xlim(3) .or.
     $    ylim(2).le.0.0 .and. ly.eq.2 .and. ylim(2).lt.ylim(3))
     $write(*,*) ' >>>> Axis limits improper for log scale'
      write(*, '(a,f6.2)') ' Current lettering height ', height
      if (nnotes.gt. 0) write(*,*)' Number of notes ',nnotes
      if (ncount.gt. 0) write(*,*) ' There is room for ',
     $maxxy-kount(1,ncount) ,' more data pairs on this plot'
      kstart=1
      do 3500 i=1,ncount
        isym=kount(2,i)
        len=kount(1,i)-kstart+1
        call minmax(len, x(kstart), xrange)
        call minmax(len, y(kstart), yrange)
        if (kount(2,i).ge.50) isym=kount(2,i)-100
        if (isym.ge.0) write(*,306)
     $  i,len,kount(4,i),isym,kount(3,i)*.01
        if (isym.lt.0) write(*,307) i,len,kount(4,i),
     $  0.01*kount(5,i), 0.01*kount(6,i)
 306    format(/' Series ',i2,'  length',i6,'  mode ',i4,
     $  '  symbol ',i4,'  size',f6.2)
 307    format(/' Series ',i2,'  length',i6,'  mode ',i4,
     $  '  dash parameters  ',2f6.2)
        write(*,310) xrange,yrange
 310    format(12x,'x range',2g12.4,'   y range',2g12.4)
        if (xrange(1).le.0.0 .and. lx.eq.2 .or.
     $      yrange(1).le.0.0 .and. ly.eq.2) write(*,*)
     $' >>>> Series range inappropriate for log scale'
        kstart=kount(1,i)+1
 3500 continue
      goto 1000
c
c ----- command  save ------
c  Saves other plot stuff.  Does not reset ncount etc
 4000 iplot=0
      goto 1000
c
c ----- command  stop ------
c  Close external file if it is open and halt gracefully
c  Flush and close plotfile if it has been written to
 6000 if (ifopen.gt.0 .and. ifile .gt. 0) close(unit=ifile)
      if (ndrawn.eq. 0) call pltsub(9)
      call ragbag(1, 0)
      stop
      end
c______________________________________________________________
      subroutine ragbag(k, n)
c$$$$ calls newpn plopen
c  This is where you start if you are converting program  plotxy
c  to another system
c
c   Changes made to adapt ragbag to 4.2BSD Unix (with special plotting
c   routines) by D. Agnew, Nov 1985
c
c  (1)  The only plotting subroutine called is  plot(x, y, ipen)
c       which is a standard Calcomp routine for moving a pen to
c       coordinates (x, y) in inches, with pen lowered (ipen=2)
c       or raised (ipen=3).  When ipen=-3 a new plot origin is defined
c       at (x, y) with respect to the previous origin.
c       These numbers are set in common /penops/ by blockdata blockb.
c       The allowed values of ipen have been expanded to include  0 and
c       4.  Ipen=0, means start a new page; while 4 which means fill
c       the last curve with solid color.  Making this act like 3 will
c       disable the fill command.
c  (2)  The ANSI FORTRAN-77 standard explicitly states that in
c           read (inp, *, end=100) (x(j), j=1, m)
c       when the end branch is taken, the index  j  is undefined.  In
c       fact the value in many compilers is  n+1  where  n  is the
c       number of values read by the statement, just as one would wish.
c       This behavior is used in  plotxy at statement 6300 of subroutine
c       readin.  If your compiler adheres to the letter of the law,
c       the only way to find out where you are is to fill your array
c       ahead of time with a check value, and after the read, inspect
c       for the overwritten portion.
c  (3)  The subroutine   remark  inserts comments into the plotfile;
c       a dummy can be provided or reference to it deleted if desired.
c
      character*64 pfile
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /paramr/ mode,x0,delx,ifopen,ifile,dash(2),laffin,affxy(4)
      common /char2/ pfile /nchr2/ npfile
      common /inout/ in,iout,idisk
c
c
      goto (10, 1000, 2000, 3000, 4000), k+1
c
c  k=0. Sets input and output unit numbers for the terminal.  In Unix
c  these are 5 and 6.  You must set  in  to the input unit number and
c  iout  to the terminal output unit number.
c  Also the external disk file for data is numbered 7 in this program
c  although your system may be more fussy.  If so change  idisk.
   10 in=5
      iout=6
      idisk=7
      ifile=idisk
      return
c
c  k=1.  Empties plot buffers and closes plot file immediately
c  prior to termination of the program.  Called from  main.
c  You must substitute plot termination calls of your system here,
c  if there are any.
 1000 call plopen(-1, pfile(1:npfile))
      return
c
c  k=2. Initializes a plot file with the name  pfile.  Called from
c  pltsub.  You must replace the following statements by those that
c  initialize plotting on your system.
 2000 call plopen(1, pfile(1:npfile))
      return
c
c  k=3.  Obsolete option deleted; there is no call with k=3
 3000 return
c
c  k=4.  In many Calcomp-compatible systems a call to newpn sets
c  color of subsequent lines according to a code.  It is assumed here
c  that  iw=1 causes black and is default.  iw=2, 3, 4 ... will give
c  other colors on those plotters with color graphics.
 4000 continue
      call newpn(max(n, 1))
      return
      end
c______________________________________________________________
      subroutine readin(list)
c$$$$ calls iochek
c  Handles those commands that read data and deals with external
c  files
      character*116 line,ifmt,image
      character*64 name,frm*11,code*4
      parameter (maxxy=75 000, maxknt=500)
      parameter (ncol=48)
      common /hues/ hsbtab(3, ncol)
      common /table/ kount(20,maxknt),ncount,ioff
      common /xylim/ ilim, kbkgrd
      common /inform/ nchar,nfound,nerr,xdata(10)
      common /char1/ code,line,image
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /char3/ name,ifmt /nchr3/ nname
      common /paramr/ mode,x0,delx,ifopen,ifile,dash(2),laffin,affxy(4)
      common // xrange(2),x(maxxy),yrange(2),y(maxxy)
      common /inout/ in,iout,idisk
      common /lnprop/ kolor,linfat
      dimension colum(40),ikon(14)
c
      save sizo,size,kind,nptx,kolx,koly,kole,frm,nfill
      data sizo,size/0.1,0.1/, kind/-1/, nptx/0/,  frm/'FORMATTED'/
      data nfill/0/
      data ikon /17,0,1,2,3,4,5,6,11,14,15,19,20,21/
c
c
      maxpts=maxxy - 2
      goto (1000,2000,3000,4000,5000,6000,7000,7500,8000,8500,
     $ 9000,9200,9300,9400,9500,9200),   list-2
c
c ----- command  file     ------
c  Copies filename into name, closes old file if necessary, opens new
c  one.  Note that if filename is '*', opening or closing is ignored.
 1000 if (nchar.ne. 0) name=line
      if (nchar.gt. 0) nname=nchar
      if (ifopen.gt. 0) close (unit=ifile)
      ifile=idisk
      if (name(1:2).eq. '* ') ifile=in
      ifopen=0
      return
c
c ----- command  format   ------
c  Read format for reading in data.  Check 1st character is OK
 2000 if (index('(*b ', line(1:1)).eq. 0) then
        write(*,'(1x,a,a74/a)') code,image,
     $' >>>> Format unacceptable: * substituted'
        ifmt='*'
        frm='FORMATTED'
      else
        ifmt=line(1:nchar)
        frm='FORMATTED'
        if (ifmt(1:1).eq. 'b') frm='UNFORMATTED'
      endif
      return
c
c ----- command  mode     ------
c  See if input is y or x values only (mode=1,-1), x-y pairs (mode=2) or
c  x-y pairs plus an error value (mode=3).  mode=-3 for error bars in x.
c  mode=4  for separate files for x and y data.
 3000 mode=xdata(1)
      if (nerr.gt.   0) goto 99999
      if (mode.eq.2 .or. abs(mode).eq. 3 .or. mode.eq. 4) return
      if (abs(mode).eq. 1) then
        x0=1.0
        if (nfound.ge. 2) x0=xdata(2)
        delx=1.0
        if (nfound.eq. 3) delx=xdata(3)
        if (delx.eq. 0.) write(*,*)' >>>> Warning: x increment is 0'
      elseif (abs(mode).eq. 10) then
        delx=1.0
        x0=1.0
        koly=1
        if (nfound.ge. 2) koly=xdata(2)
      elseif (mode.eq. 20) then
        if (nfound.lt. 3) then
          xdata(2)=1
          xdata(3)=2
        endif
        kolx=min(40.0, max(1.0, xdata(2)))
        koly=min(40.0, max(1.0, xdata(3)))
        if (kolx.ne.xdata(2).or.koly.ne.xdata(3)) write(*,*)
     $  ' >>>> Column counts illegal - reset to:',kolx,koly
      elseif (abs(mode).eq. 30) then
        if (nfound.lt. 4) then
          xdata(2)=1
          xdata(3)=2
          xdata(4)=3
        endif
        kolx=min(40.0, max(1.0, xdata(2)))
        koly=min(40.0, max(1.0, xdata(3)))
        kole=min(40.0, max(1.0, xdata(4)))
        kolz=min(40.0, max(1.0, xdata(nfound)))
        if(kolx.ne.xdata(2) .or. koly.ne.xdata(3) .or. kole.ne.xdata(4))
     $  write(*,*) ' >>>> Column counts illegal - reset to:',
     $  kolx,koly,kole
      else
        goto 99999
      endif
      return
c
c ----- command  smooth   ------
c  Checks to see if turning smooth on or off
 4000 if (line(1:2).eq. 'of') kind=max(-1, kind)
      if (line(1:2).eq. 'na' .or. nchar.eq.0) kind=-2
      if (line(1:2).eq. 'ak') kind=-3
      return
c
c ----- command  symbol   ------
 5000 if (nerr.le.   0) then
c  Analyse the numerical style first
        kind=0
        if (nfound.ge. 1) kind=xdata(1)
        if (nfound.ge.2 .and. kind.ge.0) size=xdata(2)
c  Never inherit zero size from previous setting
        if (size.eq. 0.0 .and. nfound.ne. 2) size=sizo
        if (kind.gt. 21) write(*,*)
     $  ' >>>> Symbol type too large - reduced to 21'
        kind=min(kind, 21)
      else
c  Symbols are specified by name - nerr > 0
        if (index('solfil',line(1:3)).ne. 0) then
           kind=19
           if (index(line(1:nchar),'squa').ne.0) kind=20
           if (index(line(1:nchar),'tria').ne.0) kind=21
           more=index(line(1:nchar-1),'e ')+1
           if (more.eq. 1) return
        elseif ('off' .eq. line(1:3)) then
           kind=-1
           more=0
        else
c  Symbols: circle square triangle octagon diamond plus asterisk
c           cross hexagon star dot
          ikn=index('circsqutrioctdiapluastcrohexstadot',line(1:3))
          if (ikn.eq. 0) then
            write(*,*)' >>>> Unknown symbol type: ',line(1:nchar)
            return
          endif
          kind=ikon((ikn+2)/3)
          more=index(line(1:nchar-1),' ')
        endif
        if (more.gt. 0) then
          call decode(line(more:nchar), xdata, 1, nf,mf)
          if (nf.le. 0) goto 99999
          size=xdata(1)
        else
          if (size.eq. 0.0) size=sizo
        endif
      endif
c  Save the latest nonzero size for inheritance
      if (size.ne. 0.0) sizo=size
      return
c
c ----- command  read     ------
c  Read from specified file in specified format and mode.
c  Finds how many points to read, then fusses about console input.
 6000 npts=maxpts
      if (nerr.gt.   0) goto 99999
c
      if (nfound.gt. 0) npts=xdata(1)
      if (mode .eq. -4) npts=nptx
      if (name(1:2) .eq. '* ') ifile=in
      if (ifile.eq.in .and. nchar.eq.0)      goto 99100
      if (ifile.eq.in .and. ifmt(1:1).eq.'b') ifmt(1:1)='*'
c  Open the file if not already open and it isn't console
      if (ifopen.eq.0 .and. ifile.ne.in) open(file=name(1:nname),
     $ unit=ifile, status='OLD', err=99300, form=frm)
      if (ifile.ne. in) ifopen=1
      if (iplot.ne.0) then
        ioff=1
        iplot=0
        ncount=0
      endif
      ncount=ncount+1
      if (ncount.eq.maxknt) write(*,*)
     $' >>>> Too many data series: you must PLOT before next input'
      if (ncount.gt. maxknt) goto 99200
c
c  Arrays  kount(.,.)  save various properties of current data series.
c  kount(1,.)  end of series in common
c  kount(2,.)  symbol type or interpolation style
c  kount(3,.)  symbol height in .01 inches
c  kount(4,.)  series mode
c  kount(5,.) and kount(6,.)  dash and gap lengths in .001 inches
c  kount(7,.)  color integer for series
c  kount(8,.)  simple or filled polygon
      if (ncount.eq.1) kount(1,ncount)=npts
      if (ncount.gt.1) kount(1,ncount)=kount(1,ncount-1)+npts
      kount(2,ncount)=kind
      kount(3,ncount)=int(100.*size+.5)
      kount(4,ncount)=mode
      kount(5,ncount)=int(100.*dash(1)+0.5)
      kount(6,ncount)=int(100.*dash(2)+0.5)
      kount(7,ncount)=kolor + 1000*linfat*(1-nfill)
      kount(8,ncount)=nfill
      nfill=0
c  Prepare various points for reading
      moad=abs(mode)
      if (moad.eq. 4) mode=-mode
      if (mode.eq. 4) moad=1
      kstart=1
      if (ncount.gt.1) kstart=1+kount(1,ncount-1)
      kend=kstart-1+npts
      if (moad.eq.3 .or. moad.eq.30) kend=kend+npts*2
      if (nchar.eq. 0 .and. mode.ne.4) kend=npts
      if (kend.gt. maxpts) goto 99200
c
c  Read from a table of columns.  Format forced to be *
      if (npts.le. 0) return
      if (abs(mode).eq. 10) then
        moad=1
        do 6115 i=kstart, kend
 6111     read(ifile, *, end=6400, err=6112) (colum(jj), jj=1, koly)
          y(i)=colum(koly)
          goto 6115
 6112     if (iochek(ifile, 1).eq. 0) goto 6111
          goto 99005
 6115   continue
        goto 6700
      elseif (mode.eq. 20) then
        moad=2
        maxcol=max(kolx, koly)
        do 6125 i=kstart, kend
 6120     read(ifile, *, end=6400, err=6122) (colum(jj), jj=1, maxcol)
          x(i)=colum(kolx)
          y(i)=colum(koly)
          goto 6125
 6122     if (iochek(ifile, 1).eq. 0) goto 6120
          goto 99005
 6125   continue
        goto 6700
      elseif (abs(mode).eq. 30) then
        moad=3
        maxcol=max(kolx, koly, kole, kolz)
        do 6135 i=kstart, kend, 3
 6130     read(ifile, *, end=6400, err=6132) (colum(jj), jj=1, maxcol)
          x(i)=colum(kolx)
          y(i)=colum(koly)
          y(i+1)=colum(kole)
          y(i+2)=colum(kolz)
          goto 6135
 6132     if (iochek(ifile, 1).eq. 0) goto 6130
          goto 99005
 6135   continue
        goto 6700
      endif
c  Activate next 4 lines if i is undefined after end branch in read
*     weird=-2.718e+28
*     do 6001 i=kstart, kend
*       y(i)=weird
*6001 continue
c  Formatted or binary data read. Note mode 4 always reads into y
c  Read a single binary record of data
      if (ifmt(1:1).eq. 'b') then
        if (moad.eq.3) read (ifile, end=6300)
     $   (x(i),y(i),y(i+1),i=kstart,kend,3)
        if (moad.eq.2) read (ifile, end=6300) (x(i),y(i),i=kstart,kend)
        if (moad.eq.1 .or. moad.eq.4)
     $  read (ifile, end=6300) (y(i),i=kstart,kend)
c  Read data file with * format
      elseif (ifmt(1:1).eq. '*') then
        if (moad.eq.3) read (ifile, *, end=6300, err=99000)
     $   (x(i),y(i),y(i+1),i=kstart,kend,3)
        if (moad.eq.2) read (ifile, *, end=6300, err=99000)
     $   (x(i),y(i),i=kstart,kend)
        if (moad.eq.1 .or. moad.eq.4)
     $   read (ifile, *, end=6300, err=99000) (y(i),i=kstart,kend)
c  Read data file with specified format  ifmt
      else
        if (moad.eq.3) read (ifile, ifmt, end=6300, err=99000)
     $   (x(i),y(i),y(i+1),i=kstart,kend,3)
        if (moad.eq.2) read (ifile, ifmt, end=6300, err=99000)
     $   (x(i),y(i),i=kstart,kend)
        if (moad.eq.1 .or. moad.eq.4)
     $   read (ifile, ifmt, end=6300, err=99000) (y(i),i=kstart,kend)
      endif
      goto 6700
c   Eof encountered.  Fewer points than specified - close file
 6300 continue
c  Activate next 4 lines if i is undefined after end branch in read
*     do 6002 i=kstart, kend, 1+2*(moad/3)
*       if (y(i) .eq. weird) goto 6400
*6002 continue
 6400 ifopen=0
      close (unit=ifile)
      kend=i - 1
c  Get data into its proper form according to   moad
 6700 if (mode.eq.4 .and. kend.ne.kount(1,ncount)) write(*,*)
     $' >>>> Mode 4 series of unequal length: shorter length adopted'
      kount(1,ncount)=kend
      if (maxpts .eq. kend) write(*,*)
     $' >>>> Out space for data: input series probably truncated'
      goto (6710, 6800, 6730, 6900),  moad
c  moad=1 => mode=1,4,10,-1,10
 6710 if (mode.eq. 4) goto 6800
c  Construct x values for mode 1 data
      do 6715 i=kstart,kend
        x(i)=x0 + (i-kstart)*delx
 6715 continue
      if (mode.gt. 0) goto 6800
c  When mode = -1, -10, y is evenly spaced and x input: swap x and y
      do 6720 i=kstart, kend
        xi=y(i)
        y(i)=x(i)
        x(i)=xi
 6720 continue
      goto 6800
c  moad=3 => mode=3,30,-3,-30:  construct y or x error bars according
c  to sign of  mode
 6730 flip=(1 - sign(1, mode))/2
      flop=1.0 - flip
      do 6735 i=kstart,kend,3
        if (kole.ne. kolz .and. mode.eq. 30) then
c  Asymmetric y error bars
          x(i+1)=x(i)
          x(i+2)=x(i)
        elseif (kole.ne. kolz .and. mode.eq. -30) then
c  Asymmetric x errors
          x(i+1)=y(i+1)
          x(i+2)=y(i+2)
          y(i+1)=y(i)
          y(i+2)=y(i)
        else
c  Symmetric errors bars in x or y
          x(i+1)=x(i) + y(i+1)*flip
          x(i+2)=x(i) - y(i+1)*flip
          y(i+2)=y(i) + y(i+1)*flop
          y(i+1)=y(i) - y(i+1)*flop
        endif
 6735 continue
c  Add 100 to symb if y errors, 200 for x errors
      kount(2,ncount)=kount(2,ncount) + 150 - sign(50,mode)
c
c  Perform affine transformation of x and y if so indicated
 6800 if (laffin.eq.0) return
      do 6850 i=kstart, kend
        x(i)=affxy(1)*x(i) + affxy(2)
        y(i)=affxy(3)*y(i) + affxy(4)
 6850 continue
      return
c  moad=4 => mode=4:  when x data of mode 4 have been read,
c  reset ncount for y data
 6900 ncount=ncount - 1
      nptx=kend - kstart + 1
c  Copy x data from y vector - deposited there in 1st pass of mode 4
      do 6950  j=kstart, kend
        x(j)=y(j)
 6950 continue
      return
c
c ----- command  cancel   ------
 7000 nn=1
      if (nerr.gt.   0) goto 99999
      if (nfound.ge. 1) nn=xdata(1)
      ncount=max(0,ncount-nn)
      return
c
c ----- command  logxy    ------
c  Assign log axes or not. logxy=0 means linlin, logxy=1 linxlogy,
c  logxy=2 logxliny, logxy=3 logxlogy; also logxy=4 or equilin
c  means linlin plus true proportions in x and y
 7500 if (nfound.eq. 1) logxy=xdata(1)
      if (nfound.eq. 0) 
     $logxy=index('inlinoglininlogoglogquili',line(2:6))/5
      if (line(1:1).eq. ' ') logxy=3
      jtrue=logxy/4
      if (jtrue.eq. 1) logxy=0
      return
c
c ----- command  dash     ------
c  Read parameters for dashed lines.  1st param is length of dark,
c  2nd length of gap
 8000 dash(1)=0.20
      dash(2)=0.10
      do 8010 i=1, min(4, nfound)
        dash(i)=xdata(i)
 8010 continue
      if (kind.ge.0) kind=-1
      return
c
c ----- command affine  ------
 8500 laffin=nchar
      do 8510 i=1, min(4, nfound)
        affxy(i)=xdata(i)
 8510 continue
      if (nfound.lt. 0) goto 99999
      return
c
c ----- command skip     -------
c  Skips  nskip  records in current file
 9000 nskip=1
      if (nerr.gt.   0) goto 99999
      if (nfound.eq. 1) nskip=xdata(1)
      if (ifopen.eq.0 .and. ifile.ne.in) open(file=name(1:nname),
     $ unit=ifile, status='OLD', err=99300, form=frm)
      if (ifile.ne. in) ifopen=1 + ifopen
      do 9050 i=1, nskip
        if (ifmt(1:1).eq.'b') read (ifile, end=9100) dum
        if (ifmt(1:1).ne.'b') read (ifile, '(1x)', end=9100)
 9050 continue
      return
 9100 write(*,*)
     $' >>>> End of file reached while skipping: file has been rewound'
      ifopen=0
      close( unit=ifile)
      return
c
c ----- command color (list=14), background (list=18)     -------
 9200 k=xdata(1)
      ko=1
      if (nerr.gt.   0) then
        li=(2+index('darlig',line(1:3)))/3
        ki=index(line(1:nchar-1),' ') + 1
        ko=index('blaredblugrebroorayelpurgrawhi',line(ki:ki+2))
        if (ko.eq.0) write(*,*)' >>>> Unknown color: ',line(1:nchar)
        if (ko.eq. 0) ko=24
        k=min(1, ko)*(1 + ko/3 + 11*li)
      endif
      if (list.eq. 14) kolor=k + 1000*(kolor/1000)
      if (list.eq. 18) kbkgrd=k + min(0, ko-1)
      return
c
c ----- command weight (PostScript only)    -------
c  Line weight is coded into line color as 1000*wt
 9300 if (nfound.ge. 1) iwt=xdata(1) + 0.5
      if (nerr.gt. 0 .or. iwt.lt. 0) 
     $  write(*,*)' >>>> Inappropriate line weight: ',line(1:nchar)
      if (iwt.le. 0 .or. nfound.eq.0) iwt=6
      kolor=iwt*1000 + mod(kolor, 1000)
      linfat=0
      if (nfound.eq. 2) linfat= xdata(2) - xdata(1) + 0.5
      return
c
c ----- command fill (PostScript only)    -------
 9400 nfill=1
      return
c
c ----- command palette (PostScript only) ------
 9500 if (nfound.ne. 4) then
        write(*,*)' >>>> Palette must be followed by 4 numbers'
       else
         kolo=min(ncol, max(1, nint(xdata(1))))
         hsbtab(1,kolo)=xdata(2)
         hsbtab(2,kolo)=xdata(3)
         hsbtab(3,kolo)=xdata(4)
       endif
       return
c
c                    Error section
c  Error during reading.  Print explanatory message
99000 num=iochek(ifile, 0)
99005 ncount=ncount - 1
      write(*,*) '>>>> Read error at point',
     $ i-kstart+1,'  Series',ncount+1,' rejected'
      return
c  Attempt to read from terminal to an endfile
99100 write(*,*)'>>>> Number of points must be explicit if file = *'
      return
c  Too many data or series names
99200 write(*,'(a/a,i5)')
     $' >>>> Out of space for series names or data: command ignored',
     $'      Data series number:',ncount
      return
c  File error in open statement
99300 write(*,*)
     $'>>>> File ',name(1:nname),' is nonexistent ',
     $'or you lack read permission'
      return
c  Command field can not be decoded.  Print error message
99999 write(*,'(1x,a,a72/a)') code,image,
     $' >>>> Improper command format'
      return
      end
c______________________________________________________________
      function iochek(ifile, kom)
c$$$$ calls nothing
c  If allowed, backspaces input file, checks for comment status
c  (if kom = 1), and prints the faulty record
      common /inout/ in,iout,idisk
      character*40 mark
c
      iochek=0
      if (ifile .ne. in) then
        backspace (unit = ifile)
        read (ifile, '(a)') mark
        if (mark(1:1) .eq. '%' .and. kom.eq.1)  return
        write(*, *)' >>>> Unreadable line: ',mark
      endif
      iochek=10
      return
      end
c______________________________________________________________
      subroutine pltsub(list)
c$$$$ calls plotit minmax offs plot ragbag scribe
c  Handles those commands that deal with graph appearance; also
c  executes the plot
      character*116 line,ixlab,iylab,itit,image
      character*64 pfile,note*80,tnote*80,morsel*16
      character*4 code
      parameter (maxxy=75 000, maxnot=51, maxsel=600, maxknt=500)
      common /xyaxes/ nux,nuy,nax,nay,right,top,ixe,iye
      common /inform/ nchar,nfound,nerr,xdata(10)
      common /char1/ code,line,image
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /char2/ pfile /nchr2/ npfile
      common /inout/ in,iout,idisk
      common // xrange(2),x(maxxy),yrange(2),y(maxxy)
      common /xylim/ ilim, kbkgrd
      common /paramr/ mode,x0,delx,ifopen,ifile,dash(2),laffin,affxy(4)
      common /paramp/ xlim(4),ylim(4),nxlab,nylab,ntit,hxlab,hylab,htit,
     $   height,angle,lw,iframe,kolab(3)
      common /char4/ ixlab,iylab,itit
      common /notes/ nnotes,xyh(9,maxnot),leng(maxnot),kolnot(maxnot),
     $   jot1,xynote(2,maxsel),kolmrs(maxsel),hmrs(maxsel)
      common /char5/ note(maxnot),tnote,morsel(maxsel)
      common /table/ kount(20,maxknt),ncount,ioff
      common /backs/ backy,ht,hgt,htix
      common /lnprop/ kolor,linfat
      common /penops/ iup,idn,mvor
      common /post/ boxx,boxy,land,infill
      dimension ox(2),oy(2)
      data below/0/
c
      save ior,xdist,xspace,yspace,ox,oy,idown,below
c
      goto (1000,2000,3000,4000,5000,6000,7000,8000,9000,
     $     10000,11000,12000), list
c
c ----- command  xlim     ------
c  Sets length and sets and orders limits for x
 1000 if (nfound.le.0 .or. nfound.eq.2) goto 99999
      xlim(4)=0
      do 1010 i=1, min(4,nfound)
        xlim(i)=xdata(i)
 1010 continue
      if (xlim(1).eq. 0.0) xlim(1)=3.0
      return
c
c ----- command  ylim     ------
c  Sets length and sets and orders limits for y
 2000 if (nfound.le.0 .or. nfound.eq.2) goto 99999
      ylim(4)=0
      do 2010 i=1, min(4,nfound)
        ylim(i)=xdata(i)
 2010 continue
      if (ylim(1).eq. 0.0) ylim(1)=3.0
      return
c
c ----- command  xlabel   ------
c Reads xlabel. fills rest of field with blanks
 3000 nxlab=nchar
      hxlab=abs(height)
      kolab(1)=kolor
      if (nchar.gt. 0) ixlab=line(1:nchar)
      return
c
c ----- command  ylabel   ------
c  Reads ylabel and fills field out with blanks
 4000 nylab=nchar
      hylab=abs(height)
      kolab(2)=kolor
      if (nchar.gt. 0) iylab=line(1:nchar)
      return
c
c ----- command  title    ------
c  Reads title filling out field with blanks
 5000 ntit=nchar
      htit=abs(height)
      kolab(3)=kolor
      if (nchar.gt. 0) itit=line(1:nchar)
      return
c
c ----- command  output   ------
c  Set name of plotfile.  If ndrawn>=1 file has already been
c  opened; it must be closed and flushed
 6000 if (ndrawn.ge. 1) call ragbag(1,0)
      if (nchar.gt. 0) then
        pfile=line(1:nchar)
        npfile=nchar
      endif
      ndrawn=0
      return
c
c ----- command landscape (PostScript only)   ------
c  Toggle the state of the landscape flag (it's either 0 or 1)
 7000 land=1 - land
      return
c
c ----- command character ------
c  Sets char size for title,labels,axes & notes.  Also angle of notes
 8000 if (nerr.gt. 0) goto 99999
      if (nfound.ge.1) height =xdata(1)
      if (nfound.ge.2) angle=xdata(2)
      return
c
c ----- command  plot     ------
c  Plot executes immediately without saving xdata, etc
 9000 if (ncount.eq. 0)write(*,*)' >>>> No data points in this plot'
c
      abyl=abs(ylim(1))
      ab2h=abs(2.0*height)
      under=ab2h*nux + 2.0*min(nxlab,1)*hxlab
c  1st plot or new output file; initialize plotting
      if (ndrawn.eq. 0) then
        xdist=0.0
        xspace=0.5*(8.5 - abs(xlim(1)))
        if (index(line(1:nchar),'lef').gt. 0) 
     $    xspace=0.5+2.5*(min(nylab,1)*hylab+nay*nuy*(1-right)*ab2h)
        yspace=0.5 + ab2h
        ox(2)=0.0
        oy(2)=0.0
        ior=1
        idown=0
        call ragbag(2, 0)
      endif
      ndrawn=ndrawn + 1
      iplot=1
      if (index(line(1:nchar),'dow').gt. 0)idown=1
c  Choose new origin 
c  plot ox oy:  relative origin provided
      if (nfound.eq. 2) then
        ox(ior)=ox(3-ior) + xdata(1)
        oy(ior)=oy(3-ior) + xdata(2)
c  plot ox oy abs:  absolute origin provided
      elseif (nfound.eq.0 .and. nerr.eq.3 .and.
     $  index(line(1:nchar),'abs').gt.0) then
        ox(ior)=xdata(1)
        oy(ior)=xdata(2)
c  plot top: next plot forced to top of page 
      elseif (index(line(1:nchar),'top').gt. 0) then
        if (ndrawn.gt. 1) xdist=xdist+backy+2.5*min(nylab,1)*hylab
        ox(ior)=ox(3-ior) + xspace + xdist
        oy(ior)=11.0-abyl-2.0*min(ntit,1)*htit-top*under-1.25
        xdist=0.0
        idown=1
c  plot centered
      elseif (index(line(1:nchar),'cen').gt. 0) then
        ox(ior)=ox(3-ior) + 0.5*(8.5 - abs(xlim(1)))
        oy(ior)=oy(3-ior) + 0.5*(11. - abyl)
c  Next plot below last one
      elseif (idown.eq. 1) then
        yspace=below+top*under+2*min(ntit,1)*htit+abyl+ab2h
        ox(ior)=ox(3-ior)
        oy(ior)=oy(3-ior) - yspace
      else
c  Default with no parameters: initial plot at page  bottom; subsequent
c  graphs go up the page
        ox(ior)=ox(3-ior) + xspace
        oy(ior)=oy(3-ior) + yspace + (1-top)*under
      endif
      below=(1-top)*under
c
c  Move origin
      call plot(ox(ior)-ox(3-ior), oy(ior)-oy(3-ior), mvor)
c
c  Plot everything
      call plotit
c
c  Save spacings for next stacked plot
      xdist= max(xdist, abs(xlim(1))+ ab2h)
      xspace=0.0
      yspace=2*min(ntit,1)*htit + top*under + ab2h + abyl
      ior=3 - ior
c  Restore undefined axis lengths
      if (ilim.gt. 1) ylim(1)=-4.0
      if (mod(ilim,2).eq. 1) xlim(1)=-4.0
      return
c
c ----- command  offset  --------
10000 write(*,*) ' >>>> Command offset no longer available'
      return
c
c ----- command  frame  -------
11000 if (nchar.eq. 0) then
        iframe=1
        return
      endif
      if (index(line(1:nchar),'off').ne.0) then
        iframe=0
        nax=1
        nay=1
      endif
      if (index(line(1:nchar),'on' ).ne.0) iframe=1
      if (index(line(1:nchar),'non').ne.0) then
        iframe=-1
        nax=0
        nay=0
      endif
      if (index(line(1:nchar),'top').ne.0) top=1
      if (index(line(1:nchar),'bot').ne.0) top=0
      if (index(line(1:nchar),'rig').ne.0) right=1
      if (index(line(1:nchar),'lef').ne.0) right=0
      if (index(line(1:nchar),'-xn').ne.0) nux=0
      if (index(line(1:nchar),'+xn').ne.0) then
        nux=1
        nax=1
      endif
      if (index(line(1:nchar),'-yn').ne.0) nuy=0
      if (index(line(1:nchar),'+yn').ne.0) then
        nuy=1
        nay=1
      endif
      if (index(line(1:nchar),'-xa').ne.0) nax=0
      if (index(line(1:nchar),'+xa').ne.0) nax=1
      if (index(line(1:nchar),'-ya').ne.0) nay=0
      if (index(line(1:nchar),'+ya').ne.0) nay=1
      if (index(line(1:nchar),'-bo').ne.0) iframe=0
      if (index(line(1:nchar),'+bo').ne.0) iframe=1
      if (index(line(1:nchar),'gri').ne.0) then
        iframe=2
        if (index(line(1:nchar),'sol').ne.0) iframe=3
        if (index(line(1:nchar),'-gri').ne.0)iframe=1
        nax=1
        nay=1
      endif
      if (index(line(1:nchar),'+xe').ne.0) ixe=1
      if (index(line(1:nchar),'-xe').ne.0) ixe=0
      if (index(line(1:nchar),'+ye').ne.0) iye=1
      if (index(line(1:nchar),'-ye').ne.0) iye=0
      return
c
c -----  command  note -----
c  Decodes  line  for x-y coords and text for a note, stored in /notes/
12000 if (nchar.eq.0) then
        nnotes=0
        jot1=0
        xyh(5,maxnot)=0
        return
      endif
      n1=min(maxnot-1, nnotes+1)
      xyh(5,n1)=abs(height)
      xyh(6,n1)=angle
      kolnot(n1)=kolor
      call scribe(nchar, line(1:nchar), xyh(1,n1), leng(n1), note(n1))
      if (leng(n1).ne. -1) then
        nnotes=n1
      else
c  Read a file of short notes.  Color and height saved at list end
        notfil=idisk+1
        open(unit=notfil, file=line(1:nchar), status='OLD', err=99999)
        do 12100 jot=jot1+1, maxsel
          read (notfil,'(a80)', end=12200) tnote
          call decode(tnote, xynote(1,jot), 2, nfound, nuff)
          if (nfound.ne. 2) goto 12150
          morsel(jot)=tnote(nuff+1:nuff+16)
          kolmrs(jot)=kolor
          hmrs  (jot)=abs(height)
12100   continue
        write(*,*)
     $' >>>> Notepad full: no more notes may be read from a notefile'
        jot1=maxsel
        close(unit=notfil)
        return
12150   write(*,'(a,i5/a,a)') ' >>>> Error in notefile, line ',
     $  jot-jot1,'  Offending line:',tnote
12200   jot1=jot-1
        close(unit=notfil)
      endif
      return
c
c  Unable to decode command field.  Print error message
99999 write(*,'(1x,a,a72/a)') code,image,
     $' >>>> Improper command format'
      return
      end
c______________________________________________________________
      subroutine scribe(nchar, line, xyh, leng, note)
c$$$$ calls decode
c  The string in  line  is expected to be of the form '(x,y) text'  or
c  '(x,y t) text' or '(x,y,u,v t)'.  Extracts numbers and puts text
c  into  note.  t is one or more of the letters i,c,r,f: i coords in
c  inches; c,r center or right justify, f fill arrow head
c
c  xyh(1),xyh(2): coords of head; xyh(3),xyh(4): coords of note start
c  xyh(5): height of note; -ve coords are inches not graph units
c  xyh(6); note angle, deg
c  xyh(7): 0,0.5,1 left, center, right justify; 2 append; 3.0 below,
c          3.1 centered, 3.2 right justified
c  xyh(8): 0, open, 1 filled arrowhead
      character*80 note, line*(*)
      common /inout/ in,iout,idisk
      dimension xyh(*)
c
      kpi(i)=i + 1000 -sign(1000, i-1)
      leng=-1
c  Seeks ')' in string.  Also records presence of 'i' meaning inches
c  by reversing sign of  xyh(5)
      i1=index(line, '(')
      i2=index(line, ')')
c  Check for continuation code
      nfound=0
      if (line(i1:i2).eq.'(+)')  then
        xyh(7)=2.0
        xyh(1)=999.0
        xyh(2)=xyh(1)
        goto 1100
c  Continues vertically below, left (v), center (vc), or right (vr)
      elseif (line(i1:i1+1).eq.'(v') then
        xyh(7)=3.0 + 0.1*index('cr',line(i1+2:i1+2))
        goto 1100
      endif
      if (i1.eq.0 .or. i1.gt.i2) return
      ii=index(line(i1:i2), 'i')
      ir=index(line(i1:i2), 'r')
      ic=index(line(i1:i2), 'c')
      jf=index(line(i1:i2), 'f')
      if (ii.ne. 0) xyh(5)=-xyh(5)
      ii=min (i2, kpi(ii), kpi(ir), kpi(ic), kpi(jf))
c  For left, center, right justification xyh(7)=0, 0.5, 1
      xyh(7)=0.5*min(2, min(ic,1)+min(ir,2))
      xyh(8)=min(jf,1)
c  Translate up to 4 numbers found in parentheses in  line
      call decode(line(i1+1:ii-1), xyh, 4, nfound, nuff)
      if (nfound.lt. 2) return
 1100 if (nfound.ne. 4) then
        xyh(3)=xyh(1)
        xyh(4)=xyh(2)
      endif
c  Copy remaining portion of line into character variable  note
      note=line(i2+1:nchar)
      leng=min(80, nchar - i2)
      return
      end
c______________________________________________________________
      subroutine plotit
c$$$$ calls axes draw expand justy letter lgaxes logit minmax
c$$$$ plot ragbag remark revers splot
c  Organizes plotting of data series and axes; plots title, labels,
c  notes and frame of graph
      character*116 ixlab,iylab,itit,note*80,tnote*80,morsel*16,rem*4
      parameter (maxxy=75 000, maxnot=51, maxsel=600, maxknt=500)
      dimension ss(4),p(4)
      common /xyaxes/ nux,nuy,nax,nay,right,top,ixe,iye
      common // xrange(2),x(maxxy),yrange(2),y(maxxy)
      common /xylim/ ilim, kbkgrd
      common /paramp/ xlim(4),ylim(4),nxlab,nylab,ntit,hxlab,hylab,htit,
     $   height,angle,lw,iframe,kolab(3)
      common /char4/ ixlab,iylab,itit
      common /notes/ nnotes,xyh(9,maxnot),leng(maxnot),kolnot(maxnot),
     $   jot1,xynote(2,maxsel),kolmrs(maxsel),hmrs(maxsel)
      common /char5/ note(maxnot),tnote,morsel(maxsel)
      common /backs/ backy,ht,hgt,htix
      common /table/ kount(20,maxknt),ncount,ioff
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /penops/ iup,idn,mvor
      common /kink/ fill(2),xo,yo,snext,ink
      common /inout/ in,iout,idisk
      common /lnprop/ kolor,linfat
      common /post/ boxx,boxy,land,infill
      common /grdlog/ logik,xslg,ngrx,ngry,grxy(200)
c
      save ixrev,iyrev, p3,p4, ss2,ss3
      data ixrev,iyrev /1,1/, logx,logy/0,0/, ss2,ss3/0,0/
      data cosa,tana,tip/0.866,0.577,0.6/, zone/1.2/, p3,p4/0,0/
c
      ht=abs(height)
      hgt=max(0.0, height)
      htix=0.667*ht
      nxy=0
      if (ncount.gt. 0) nxy=kount(1,ncount)
c
c  Check extremes of plot, assign plotting interval, overruling request
c  for log scales if nonpositive values are found.  Convert data to logs
c  as necessary and draw appropriate axes.
c  Reverse sign of data if limits are in reverse order
      if (xlim(2) .gt. xlim(3)) call revers(nxy, x, xlim(2), ixrev)
      if (ylim(2) .gt. ylim(3)) call revers(nxy, y, ylim(2), iyrev)
c
      call minmax(nxy, x, xrange)
      call minmax(nxy, y, yrange)
      ilim=1.501 - sign(0.5, xlim(1)) - sign(1.0, ylim(1))
c  Adjust ranges to make a true proportion plot
      if (jtrue.eq. 1) then
        if (ylim(1).lt. 0) ylim(1)=abs(xlim(1))
        if (xlim(1).lt. 0) xlim(1)=abs(ylim(1))
        if ((yrange(2)-yrange(1))/ylim(1) .ge. 
     $      (xrange(2)-xrange(1))/xlim(1)) then
          call expand(logxy/2, ylim, yrange, logy)
          wdx=(yrange(2)-yrange(1))*xlim(1)/ylim(1)
          xrange(2)=(xrange(2)+xrange(1) + wdx)/2.0
          xrange(1)=xrange(2) - wdx
        else
          call expand(mod(logxy,2), xlim, xrange, logx)
          wdy=(xrange(2)-xrange(1))*ylim(1)/xlim(1)
          yrange(2)=(yrange(2)+yrange(1) + wdy)/2.0
          yrange(1)=yrange(2) - wdy
        endif
      else
c  Adjust ranges to fill plot window provided
        call expand(mod(logxy,2), xlim, xrange, logx)
        call expand(logxy/2,      ylim, yrange, logy)
      endif
      if (logxy .ne. logx+2*logy) write(*,'(a)') ' >>>> Data'//
     $' nonpositive or plot limits improper:  log scale not provided.',
     $' To identify offending item, use status command',' '
c  If both axis lengths are negative invent suitable dimensions
c  Attempts to keep true scale within reasonable aspect ratios
      rat=(yrange(2)-yrange(1))/(xrange(2)-xrange(1))
      if (rat+1/rat .gt. 3.0) rat=1
      if (ylim(1).lt. 0 .and. xlim(1).lt. 0) then
         if (rat.le. 1) xlim(1)=5.0
         if (rat.ge. 1) ylim(1)=5.0
      endif
c  Default graph width and height set here
      if (ylim(1).lt. 0) ylim(1)=min(xlim(1)*rat, 6.0)
      if (xlim(1).lt. 0) xlim(1)=min(ylim(1)/rat, 5.0)
c
c  Plot title.  This may set font for axis numerals
      call ragbag(4, kolab(3))
      if (ntit.gt. 0) then
        call remark('Title follows: '//itit(1:10)//' ...')
        call justy(ss, htit, itit(1:ntit))
        uptit=ylim(1)+htit+2*top*(nux*nax*hgt+min(1,nxlab)*hxlab)
        call letter(0.5*xlim(1)-ss(2), uptit, htit, 0.0, itit(1:ntit))
      endif
c
c  Fill in color background if requested
      if (kbkgrd.gt. 0) then
        call ragbag(4, kbkgrd)
        call plot(0.0, ylim(1), iup)
        call plot(xlim(1), ylim(1), idn)
        call plot(xlim(1), 0.0,     idn)
        call plot(0.0,     0.0,     idn)
        call plot(0.0, 0.0, 4)
      endif
c
c  Plot axes, grids and frame
      call ragbag(4, kolor)
c
      if (nax.gt.0 .or. nay.gt.0) call remark('Plotting axes')
      if (nax.gt. 0) then
        if (top.ne.0) call plot(0.0, ylim(1), mvor)
        if (logx.eq.0) call   axes(ixrev*xlim(1), xrange, xlim(4), 1)
        if (logx.eq.1) call lgaxes(xlim(1), xrange, 1, xlim(4))
        if (top.ne.0) call plot(0.0, -ylim(1), mvor)
      endif
      if (nay.gt. 0) then
        if (right.ne.0) call plot(xlim(1), 0.0, mvor)
        if (logy.eq.0) call   axes(iyrev*ylim(1), yrange, ylim(4), 2)
        if (logy.eq.1) call lgaxes(ylim(1), yrange, 2, ylim(4))
        if (right.ne.0) call plot(-xlim(1), 0.0, mvor)
      endif
      if (logx.eq.1) call logit(1, nxy+2, xrange)
      if (logy.eq.1) call logit(1, nxy+2, yrange)
c
c  Draw a grid if requested; data will fall on top of grid lines
      if (iframe.ge. 2) then
        call ragbag(4, kolor)
        call remark('Plotting grid lines')
        fill(1)=0.01
        fill(2)=0.05
c  Detect solid-grid
        if (iframe.ge. 3) fill(1)=0.0
        if (iframe.eq.3 .and. mod(kolor,100).le.1) call ragbag(4,11)
        gap=0.05
        do 1200 i=1, ngrx
          call draw(0.0,     0.0, 1)
          call draw(grxy(i), gap,     iup)
          call draw(grxy(i), ylim(1)-gap, idn)
 1200   continue
        do 1300 i=1, ngry
          call draw(0.0,     0.0, 1)
          call draw(gap,     grxy(i+ngrx), iup)
          call draw(xlim(1)-gap, grxy(i+ngrx), idn)
 1300   continue
        if (iframe.eq. 3) call ragbag(4, kolor)
      endif
c
      if (xlim(1)*ylim(1)*ncount.eq. 0.0) goto 2000
c
c  Plot each data series according to its specification
      j=1
      do 1500 k=1, ncount
        write(rem,'(i4)') k
        call remark('Plotting data series '//rem)
        if (kount(2,k).lt. 100) infill=kount(8,k)
        call ragbag(4, kount(7, k))
        fill(1)=0.01*kount(5,k)
        fill(2)=0.01*kount(6,k)
        call splot(k, kount(1,k)-j+1, x(j), y(j), xrange, yrange,
     $  kount(2,k), kount(3,k))
        if (infill.eq. 1) call plot(0.0, 0.0, 4)
        j=kount(1,k) + 1
        infill=0
 1500 continue
c
c  Restore logarithmic data to original values
      if (logx.eq.1) call logit(-1, nxy, x)
      if (logy.eq.1) call logit(-1, nxy, y)
c
c  Plot axis labels properly centered
 2000 if (nylab.gt. 0) then
        call remark('Plotting ylabel: '//iylab(1:10)//' ...')
        call justy(ss, hylab, iylab(1:nylab))
        call ragbag(4, kolab(2))
        space=(right-1)*(hylab+nuy*nay*backy)+
     $  right*(xlim(1)+2*hylab+nuy*nay*backy)
        call letter(space, 0.5*ylim(1)-ss(2),
     $             hylab, 90., iylab(1:nylab))
      endif
      if (nxlab.gt. 0) then
        call remark('Plotting xlabel: '//ixlab(1:10)//' ...')
        call justy(ss, hxlab, ixlab(1:nxlab))
        call ragbag(4, kolab(1))
        space=2*(top-1)*(hxlab+nux*nax*hgt) +
     $           top*(ylim(1)+hxlab+2*nux*nax*hgt)
        call letter (0.5*xlim(1)-ss(2), space,
     $               hxlab, 0.0, ixlab(1:nxlab))
      endif
c
c  Frame picture if requested.  If axes omitted, supply necessary sides
      if (iframe.gt.0) then
        call remark('Plotting frame')
        call ragbag(4, kolor)
        call plot(0.0, ylim(1), iup)
        call plot(xlim(1), ylim(1), idn)
        call plot(xlim(1), 0.0,     idn)
        call plot(0.0,     0.0,     idn)
        call plot(0.0, ylim(1),     idn)
      endif
c
c  Plot notes file and arrows
 3000 if (nnotes.eq.0 .and. jot1.eq.0) goto 5000
      do 3600 n=1, nnotes
        tnote=note(n)
        long=leng(n)
        call remark('Plotting note: '//tnote(1:long))
        call ragbag(4, kolnot(n))
        do 3100 i=1, 4
          p(i)=xyh(i,n)
 3100   continue
        hn=abs(xyh(5,n))
        if (long.eq.1 .and. tnote(1:1).eq.' ') hn=0.0
        call justy(ss, hn, tnote(1:long))
c  Coords in inches
        if (xyh(5,n).lt. 0.0) goto 3300
c  Notes continue from previous line, appended or below
        if (xyh(7,n).eq. 2.0) goto 3400
        if (nint(xyh(7,n)).eq. 3) then
          p(3)=p3
          if (xyh(7,n).gt. 3.09) p(3)=p3 - ss(2) + ss2
          if (xyh(7,n).gt. 3.19) p(3)=p3 - ss(3) + ss3 
          p(4)=p4 - 1.8*hn
          goto 3400
        endif
        do 3200 i=1, 3, 2
          if (logx.eq.1) p(i  )=log10(max(p(i  ), 1.2e-38))
          if (logy.eq.1) p(i+1)=log10(max(p(i+1), 1.2e-38))
          p(i  )=(p(i  )*ixrev-xrange(1))*xlim(1)/(xrange(2)-xrange(1))
          p(i+1)=(p(i+1)*iyrev-yrange(1))*ylim(1)/(yrange(2)-yrange(1))
 3200   continue
c  Recenter text according to justification in xyh(7,:)
 3300   p(3)=p(3) - xyh(7,n)*ss(3)*cos(0.017453*xyh(6,n))
        p(4)=p(4) - xyh(7,n)*ss(3)*sin(0.017453*xyh(6,n))
 3400   call remark('Note text follows')
        call letter(p(3), p(4), hn, xyh(6,n), tnote(1:long))
        p3=p(3)
        p4=p(4)
        ss2=ss(2)
        ss3=ss(3)
        if (p(1).eq.p(3) .and. p(2).eq.p(4)) goto 3600
        if (xyh(1,n).eq.xyh(3,n).and.xyh(2,n).eq.xyh(4,n)) goto 3600
c  Plot arrow if there is one; letter ht<0 means no tip
        p5=p(3) + ss(2)  - p(1)
        p6=p(4) + 0.3*hn - p(2)
        el=ss(3) - ss(2) + 0.5*hn
        te=p5 - p5*(el - hn)/(1.0e-6 + el + abs(p5))
        p8=p6 - sign(zone*hn, p6)
        p7=te*p8/(p6 + sign(1.0e-6, p6))
        if (abs(p7 - p5) .gt. el) then
          p7=p5 - sign(el, p5)
          p8=p6*p7/te
        endif
        c=tip*cosa*abs(xyh(5,n))/sqrt(p7**2 + p8**2)
        s=tana*c
        call plot(p(1) + c*p7 + s*p8, p(2) - s*p7 + c*p8, iup)
        call plot(p(1),               p(2),               idn)
        call plot(p(1) + c*p7 - s*p8, p(2) + s*p7 + c*p8, idn)
        infill=xyh(8,n)
        call plot(p(1),               p(2),               iup)
        infill=0
        call plot(p(1)+p7,            p(2)+p8,            idn)
        call remark('Arrow finished')
 3600 continue
c  Short notes from a file
      do 3700 jot=1, jot1
        p(1)=xynote(1,jot)
        p(2)=xynote(2,jot)
        if (logx.eq.1) p(1)=log10(max(p(1), 1.2e-38))
        if (logy.eq.1) p(2)=log10(max(p(2), 1.2e-38))
        p(1)=(p(1)*ixrev-xrange(1))*xlim(1)/(xrange(2)-xrange(1))
        p(2)=(p(2)*iyrev-yrange(1))*ylim(1)/(yrange(2)-yrange(1))
        call remark('Plotting a short note')
        call ragbag(4, kolmrs(jot))
        call letter(p(1), p(2), hmrs(jot), 0.0, morsel(jot))
 3700 continue
c
c  Restore sign of data if limits are in reverse order
 5000 if (ixrev.eq. -1) call revers(nxy, x, xlim(2), ixrev)
      if (iyrev.eq. -1) call revers(nxy, y, ylim(2), iyrev)
      return
      end
c______________________________________________________________
      subroutine splot(nu, n, x, y, xl, yl, jsym, jsize)
c$$$$ calls akima draw eval evlak glyph plot sort2 spline nvelop
c  Plots a single continuous curve, or a smoothed continuous curve, or a
c  symbol.  Coordinates are given by arrays x(1),y(1), ... x(n),y(n),
c  limits are xl(1) .ge. x .ge. yl(2) and similarly for y.
c  If  jsym=-1,     plot a straight line between consecutive points,
c  if  jsym=-2,     a  natural spline-smoothed curve is drawn,
c  if  jsym=-3,     an  Akima  spline-smoothed curve is drawn,
c  if  jsym.ge.0,   this is the Calcomp symbol identifier plotted.
c  if  jsym.ge.100, this is a symbol with a vertical error bar.
c  if  jsym.ge.200, this is a symbol with a horizontal error bar.
c  Then jsym-100 is Calcomp symbol number.
c  Jsize  is symbol height in .01 inches.  May be 0 for error bar.
      parameter (kspace=10000)
      dimension x(*), y(*), xl(2),yl(2),  work(kspace)
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /inout/ in,iout,idisk
      common /post/ boxx,boxy,land,infill
      common /paramp/ xlen,xlim(3),ylen,ylim(3),
     $                                nxlab,nylab,ntit,hxlab,hylab,htit,
     $   height,angle,lw,iframe,kolab(3)
      common /backs/ backy,ht,hgt,htix
      common /penops/ iup,idn,mvor
c
      if (n.eq.0) return
      if (n.eq.1 .and. jsym.lt.0) then
        jsym=5
        jsize=10
      endif
      xscale=xlen/(xl(2) - xl(1))
      yscale=ylen/(yl(2) - yl(1))
      ipen=iup
      if (jsym.ge. 0) goto 3000
      if (jsym.le.-2) goto 2000
c  Straight-line interpolation between consecutive points, discarding
c  points outside frame.  Also initialize dashed line parameters.
 1000 call draw(0.0, 0.0, 1)
      do 1500 i=1, n
        xx=xscale*(x(i) - xl(1))
        yy=yscale*(y(i) - yl(1))
        if (xx.lt.-0.001 .or. xx.gt.xlen+0.001
     $    .or.yy.lt.-0.001 .or. yy.gt.ylen+0.001) then
          ipen=iup
        else
          call draw(xx, yy, ipen)
          ipen=idn
        endif
 1500 continue
C
      if (ipen .eq. idn) call draw(xx, yy,  iup)
      return
c  Cubic-spline interpolation discarding points outside frame;
c  sort on x
 2000 call sort2(n, x, y)
c  Check for identical x values
      do 2100 i=2, n
        if (x(i).eq.x(i-1)) then
          write(*,*) ' >>>> Series',nu,' contains ',
     $    'repeated x values and so cannot be smoothed'
          goto 1000
        endif
 2100 continue
      if (2*n .gt. kspace) then
        write(*,*) ' >>>> Series',nu,' too long to be smoothed'
        goto 1000
      endif
c  Initialize scaling constants for curvatures on graph
      curve=3.0*yscale/xscale**2
      xo=-100.0
      yo=1.01*yscale*(y(1) - yl(1))
      ibegun=0
      ipen=iup
      bent=0.0
      nat=3 + jsym
c  Form natural or Akima spline interpolation tables in work
      if (nat.eq. 1 ) then
        call spline(n, x, y, work, work(n+1))
      else
        call akima(n, x, y, work(n+1))
        do 2220 i=2, n
          dx=x(i) - x(i-1)
          work(i)=(work(n+i)  - work(n+i-1))/dx 
 2220   continue
        work(1)=work(2)
      endif
      call draw(0.0, 0.0, 1)
      do 2600 i=2, n
        if (x(i).le.xl(1)) goto 2600
c  Using 2nd derivs in work, adjusts x-step to local curvature
        x1=max(xl(1), x(i-1))
        xdist=min(xl(2), x(i)) - x1
        bend=curve*max(abs(work(i-1)), abs(work(i)))
        nn=2.0 + xscale*xdist/(0.01 + 0.2/(1.0 + bend))
        dx=xdist/(nn - 1)
        bent=max(bent, bend)
        lagj=min(1, n-i)
        do 2500 j=1, nn-lagj
          xi=(j-1)*dx + x1
          xx=xscale*(xi - xl(1))
          if (nat.eq. 1) yy=yscale*(eval(xi, n, x, y, work) - yl(1))
          if (nat.ne. 1)
     $    yy=yscale*(evlak(xi, n, x, y, work(n+1)) - yl(1))
c  Interpose a point on upper or lower edge if curve moves into or
c  out of window
          do 2300 ey=0.0, ylen, ylen
            xe=(xo*(ey-yy) - xx*(ey-yo))/((yo-yy) + 1.0e-10)
            if (max(xo,xl(1)).le.xe.and.xe.le.xx) then
              call draw(xe, ey, ipen)
              ibegun=1
             endif
 2300     continue
c  Plot an ordinary point inside window
          if (-.001.le.yy.and.yy.le.ylen+.001) then
            if (ibegun.gt. 0) ipen=idn
            ibegun=1
            call draw(xx, yy, ipen)
          else
            ipen=iup
          endif
          xo=xx
          yo=yy
 2500   continue
        if (x(i).ge.xl(2)) goto 2605
 2600 continue
 2605 if (bent.gt. 2000. .and. nat.eq.1)write(*,*)' >>>> SMOOTH ',
     $'may be unsuitable for series',nu,':  Wild oscillations likely'
C
      if (ipen.eq.idn) call draw(xx, yy, iup)
      return
c
c  Plot symbol of specified number and height
 3000 size=0.01*jsize
      if (abs(size).ge. 4.0) size=0.1
      if (jsym.ge.95) goto 4000
      do 3500 i=1, n
        xx=xscale*(x(i) - xl(1))
        yy=yscale*(y(i) - yl(1))
        if (xx.gt.-0.001 .and. xx.lt.xlen+0.001 .and. yy.gt.-0.001
     $  .and. yy.lt.ylen+0.001) call glyph(xx, yy, size, jsym)
 3500 continue
      return
c  Plot specified symbol and associated y-error bar.  Extremes
c  of bar are stored in next 2 y values.
c  Decide on size of foot from size of plot, or symbol size if present
 4000 ft=min(0.05, 0.015*max(xlen, ylen))
      if (jsym.le.195) ft=max(0.0, size/2.0)
      if (jsize.ge. 1000) goto 6000
      if (jsym.gt.195) goto 5000
c  Outline uncertainty region and fill in (only works for y errors)
      if (infill.ne. 0) then
        call nvelop(n,x,y, xl,yl)
        return
      endif
c  Draw error bars with or without feet
      do 4500 i=1, n, 3
        xx=xscale*(x(i) - xl(1))
        if (xx.lt.-0.001 .or. xx.gt. xlen+0.001) goto 4500
        yy=yscale*(y(i) - yl(1))
        if (yy.gt.-0.001 .and. yy.lt.ylen+0.001 .and. size.ne.0.0
     $  .and. jsym.ge.100) call glyph(xx, yy, size, jsym-100)
c  Draw error bars and feet.  No foot if that end lies outside ylimits
        do 4400 j=1, 2
          yy=min(max(yscale*(y(i+j)-yl(1)), -0.001), ylen+0.001)
          call plot(xx,  yy, 4-j)
          if (yy.lt. 0.0 .or. yy.gt.ylen) goto 4400
          call plot(xx-ft, yy, iup)
          call plot(xx+ft, yy, idn)
          call plot(xx,    yy, idn)
 4400   continue
 4500 continue
      return
c  Plot specified symbol and associated x-error bar.  Extremes
c  of bar are stored in next 2 x values
 5000 do 5500 i=1, n, 3
        yy=yscale*(y(i) - yl(1))
        if (yy.lt.-0.001 .or. yy.gt. ylen+0.001) goto 5500
        xx=xscale*(x(i) - xl(1))
        if (xx.gt.-0.001 .and. xx.lt.xlen+0.001 .and. size.gt.0.0
     $  .and. jsym.ge.200) call glyph(xx, yy, size, jsym-200)
c  Draw error bars and feet.  No foot if that end lies outside xlimits
        do 5400 j=1, 2
          xx=min(max(xscale*(x(i+j)-xl(1)), -0.001), xlen+0.001)
          call plot(xx,     yy, 4-j)
          if (xx.lt. 0.0 .or. xx.gt.xlen) goto 5400
          call plot(xx, yy-ft, iup)
          call plot(xx, yy+ft, idn)
          call plot(xx,     yy, idn)
 5400   continue
 5500 continue
      return
c  Instead of error bars draw symbols of varying height
 6000 if (jsym.lt. 0) return
      do 6500 i=1, n, 3
        szsy=yscale*(y(i+2)-y(i))
        yy=  yscale*(y(i) - yl(1))
        if (yy.lt.-0.001 .or. yy.gt. ylen+0.001) goto 6500
        xx=xscale*(x(i) - xl(1))
        if (xx.gt.-0.001 .and. xx.lt.xlen+0.001)
     $  call glyph(xx, yy, szsy, jsym-100)
 6500 continue
      return
      end
c______________________________________________________________
      subroutine nvelop(n,x,y, xl,yl)
c$$$$ plot
c  Draws filled polygon around region whose corners are the ends of
c  the error bars read with mode=3.  Only for mode 3, 30
      dimension x(*),y(*),xl(2),yl(2)
      common /penops/ iup,idn,mvor
      common /paramp/ xlen,xlim(3),ylen,ylim(3),
     $ nxlab,nylab,ntit,hxlab,hylab,htit,
     $   height,angle,lw,iframe,kolab(3)
c
      xmap(t)=xscale*(t-xl1)
      ymap(t)=min(ylen, max(0.0, yscale*(t-yl1)))
c
      xl1=xl(1)
      yl1=yl(1)
      xscale=xlen/(xl(2) - xl(1))
      yscale=ylen/(yl(2) - yl(1))
c
      i1=4
      if (min(y(2),y(3)).gt.yl(2).or.max(y(2),y(3)).lt.yl(1)) i1=7
      do 1500 i=i1, n, 3
        if(min(y(i+1),y(i+2)).gt.yl(2).or.max(y(i+1),y(i+2)).lt.yl1.or.
     $  min(x(i),x(i-3)).lt.xl1.or.max(x(i),x(i-3)).gt.xl(2)) goto 1500
        call plot(xmap(x(i+1)),ymap(y(i+1)),  iup)
        call plot(xmap(x(i+2)),ymap(y(i+2)),  idn)
        call plot(xmap(x(i-1)),ymap(y(i-1)),  idn)
        call plot(xmap(x(i-2)),ymap(y(i-2)),  idn)
 1500 continue
c
      return
      end
c__________________________________________________________________
      subroutine axes(xleng, xx, xtin, ixy)
c$$$$ calls justy letter nicer plot etoten
c  Plots either an x- or a y-axis of length  xleng  inches with tick
c  marks at reasonable places between limits  xx(1), xx(2).
c  xtin  is a suggested interval for tick marks, which may be ignored.
c  ixy =1 means  x axis,  ixy=2  means y axis.
c  Axis is drawn starting from plot orign (x=0, y=0), which is normally
c  at lower left corner.  This position may be moved by user in calling
c  routine.  Negative axis length means reverse sign of plotted
c  numbers.
c
      character*40 label,fmat*20
      dimension xx(2),xa(2),s(4)
      common /xyaxes/ nux,nuy,nax,nay,right,top,ixe,iye
      common /grdlog/ logik,xslg,ngrx,ngry,grxy(200)
      common /backs/ backy,ht,hgt,htix
      common /penops/ iup,idn,mvor
      data ticfac/1.6/
c
      iten(x)=int(500.01 + log10(x)) - 500
      floor(x)=aint(x+1e-4) + min(0.0, sign(1.0, x))
      c125(xlog)=max(1.0, 10.**int(xlog) *
     $     nint(10.**(0.3398*int(3*mod(xlog,1.0) +1.57)-0.3495)))
c
      if (xleng.eq. 0.0) return
      xlen=abs(xleng)
      revers=sign(1.0, xleng)
      xa(1)=xx(1)
      xa(2)=min(1.0, ticfac*10.0/xlen)*(xx(2)-xx(1)) + xx(1)
      call nicer(xa, xtin, xa, xt)
c  Set up format for numerical annotation on axis
      nsiz=iten(max(abs(xx(2)), abs(xx(1))))
      ntix=iten(abs(xt))
      nfld=3 + max(nsiz, -ntix, nsiz-ntix)
c  Width of field less than 8 characters - use an f-format
      if (nfld.le.7 .or. nfld-nsiz+ntix.eq.3) then
        ix=nint(xt*10.0**(2-ntix))
        im=max(0, min(2-ntix, min(1, mod(ix,100))+min(1, mod(ix,10))))
        ndp=max(0, -ntix) + im
        nfld=nfld + im
        write(fmat, '(a,i2,a,i2,a)') '(f', nfld, '.', ndp, ')'
        if (ndp.eq. 0) nfld=nfld - 1
      else
c  Width of field more than 7 characters - use a g format
        ndp=nsiz - ntix + 1
        nfld=7 + ndp
        write(fmat, '(a,i2,a,i2,a)') '(1pg',nfld,'.',ndp,')'
      endif
c
      call plot(0.0, 0.0, iup)
      xs= xlen/(xx(2) - xx(1))
      n1=-floor(-xx(1)/xt)
      n2= floor(xx(2)/xt)
      goto (2100, 3100), ixy
c
c  Draw x axis
 2100 kskipx=c125(log10(aint(1.3 + hgt*nfld*(logik+1)/(xs*xt))))
      nlab=(n2-n1+1)/kskipx
      ngrx=0
      do 2500 n=n1, n2
        x=n*xt
        xinch=xs*(x - xx(1))
        if (logik.eq. 1) xinch=xslg*log10(x/xx(1))
c  Plot next section of axis and tick on right, shortened if no label
        call plot(xinch, 0.0,  idn)
        lab=0
        if  (nlab.ge.2 .and. mod(n, kskipx).eq.0) lab=1
        if (nlab.lt.2 .and. (n.eq.n1 .or. n.eq.n2)) lab=1
        call plot(xinch, (1-2*top)*htix/(1.6-0.6*(1-ixe)*lab), idn)
c  Save position of tick in case grid is requested
        ngrx=ngrx + 1
        grxy(ngrx)=xinch
c  Write numerical annotation
        if (lab.eq.1 .and. nux.gt.0) then
          write(label, fmat) x*revers
          call etoten(label, nfld, mfld)
          call justy(s, hgt, label(1:mfld))
          space=(3*top - 2)*hgt
          call letter(xinch-s(2), space, hgt, 0.0, label(1:mfld))
        endif
c  Move back onto axis with pen up
        call plot(xinch, 0.0, iup)
 2500 continue
c  Plot last little piece of axis 
      call plot(xlen,  0.0, idn)
      return
c
c  Draw y axis
 3100 half=-0.5*hgt
      backy=-half
      kskipy=c125(log10(aint(1.5 + hgt*(logik+1)/(xs*xt))))
      nlab=(n2-n1+1)/kskipy
      ngry=0
      pica=sign(0.5*hgt*right, min((n1+1)*revers, (n2-1)*revers))
      do 3500 n=n1, n2
        y=n*xt
        yinch=xs*(y - xx(1))
        if (logik.eq.1) yinch=xslg*log10(y/xx(1))
c  Plot next section of axis and tick on right
        call plot(0.0,  yinch, idn)
        lab=0
        if (nlab.ge.2 .and. mod(n, kskipy).eq.0) lab=1
        if (nlab.lt.2 .and. (n.eq.n1 .or. n.eq.n2)) lab=1
        call plot((1-2*right)*htix/(1.6-0.6*(1-iye)*lab), yinch, idn)
        ngry=ngry + 1
        grxy(ngrx+ngry)=yinch
c  Write numerical annotation
        if (lab.eq.1 .and. nuy.gt.0) then
          write(label, fmat) y*revers
          call etoten(label, nfld, mfld)
          call justy(s, hgt, label(1:mfld))
          backy=max(backy, s(3)-s(1)-half)
          space=(1-right)*(half-s(3)) -  pica
          call letter(space,  yinch+half, hgt, 0.0, label(1:mfld))
        endif
        call plot(0.0, yinch, iup)
 3500 continue
c  Plot last little piece of axis 
      call plot(0.0,  xlen, idn)
      return
      end
c______________________________________________________________
      subroutine etoten(label, nlab, mlab)
c$$$$ calls no other routines
c  Converts FORTRAN E-style exponential notation to proper 10^n form
c  Overwrites input string, returns new length in mlab.
c
      character*(*) label,mabel*40, bkslsh*1
c
c  Because of special status of \ in Unix, generate it from ASCII
      bkslsh=char(92)
c
      ie=index(label, 'E')
      mlab=nlab
      ip=index(label(1:nlab),'. ')
      if (ip.gt. 0) mlab=ip-1
      if (ie.eq. 0) return
c
c  Drop trailing insignificant zeros and decimal point
      do 1100 i1=ie-1, 1, -1
        if (label(i1:i1) .ne. '0') goto 1110
 1100 continue
 1110 if (label(i1:i1) .eq. '.') i1=i1 - 1
c
c  Eliminate + signs and leading 0 in exponents
      i2=ie+2
      if (label(i2-1:i2-1) .eq. '+') then
        if (label(i2:i2) .eq. '0') i2=i2+1
      else
        if (label(i2:i2) .ne. '0') then
          i2=i2-1
        else
          label(i2:i2)='-'
        endif
      endif
c
c  Compose number from its fraction and exponent
      mabel=label(1:i1)//bkslsh//'1398'//bkslsh//'10'//
     $      bkslsh//'sup{'//label(i2:nlab)//'}   '
      mlab=nlab+13
      label=mabel
      return
      end
c_______________________________________________________________________
      subroutine lgaxes(xlen, xx, ixy, tentix)
c$$$$ calls axes justy letter plot
c  Plots either an x- or a y-axis of length  xlen  inches with log
c  spaced tick marks and annotation between limits  xx(1), xx(2).
c  ixy=1  means draw an x axis,  ixy=2  means draw a y axis.
c  If tentix .lt. 0 omit all but 10**n tick marks
c  The axis is drawn starting from plot orign (x=0, y=0), which is
c  Normally at lower left corner.  This position may be moved by
c  user in calling routine.
c
      character*4 label, no*2
      dimension xx(2),s(4), no(10)
      common /xyaxes/ nux,nuy,nax,nay,right,top,ixe,iye
      common /backs/ backy,ht,hgt,htix
      common /grdlog/ logik,xslg,ngrx,ngry,grxy(200)
      common /penops/ iup,idn,mvor
      data no/'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','10'/
c
      if (xlen.eq. 0.0) return
      log1=int(500.01 + log10(xx(1))) - 500
      log2=int(500.01 + log10(xx(2))) - 500
      logd=log2 - log1
      xslg=xlen/log10(xx(2)/xx(1))
      if (log2.eq.log1 .or. xx(2)/xx(1).lt. 10.0) goto 4000
      hr=0.75*hgt
      call justy(s, hgt, no(10))
      wid10=s(3)
      call plot(0.0, 0.0, iup)
c  Select x- or y- axis
      goto (2100, 3100), ixy
c
c  Plot x axis
 2100 back=0.5*wid10
      kskip=1.1 + 5.0*hgt/xslg
      space=(3*top - 2)*hgt
      ngrx=0
      do 2500 kdec=log1, log2
        iskp10=min(mod(kdec-log1+kskip-1, kskip), 1) 
        do 2400 numb=1, 9
          xinch=xslg*(log10(numb/xx(1)) + kdec)
          if (xinch.lt.-.001 .or. xinch.gt. .001+xlen) goto 2400
c  Draw next section of axis and its tick mark. Save marker for grid
          call plot(xinch, 0.0, idn)
          if (numb.eq.1 .or. xslg.gt.4.0*hgt .and. tentix.ge.0) then
            shrnk=min(1.6, real(numb+iskp10))
            call plot(xinch, (1-2*top)*htix/shrnk, idn)
            ngrx=ngrx + 1
            grxy(ngrx)=xinch
          endif
          if (nux.gt. 0) then
c  If number of decades .lt.2 write integers next to their ticks
            if (numb.ne. 1) then
              if (logd.le. 2  .and. .02*xslg .gt. hgt)
     $        call letter(xinch-0.5*hr, space, hr, 0.0, no(numb))
c  Write out notation 10**kdec below appropriate tick
            elseif (iskp10.eq. 0) then
              write(label, '(i4)') kdec
              call justy(s, hr, label)
              call letter(xinch+back-s(1), space+.5*hgt, hr, 0.0, label)
              call letter(xinch-back,      space,      hgt, 0.0, no(10))
            endif
          endif
c  Move back onto axis with pen raised
          call plot(xinch, 0.0, iup)
 2400   continue
 2500 continue
c  Plot last piece of axis 
      call plot(xlen,  0.0, idn)
      return
c
c  Plot y axis
 3100 backy=2.0*wid10
      kskip=1.1 + 2.5*hgt/xslg
      ngry=0
      space=hr*(2*right -1.5)
      do 3500 kdec=log1, log2
        iskp10=min(mod(kdec-log1, kskip), 1)
        do 3400 numb=1, 9
          xinch=xslg*(log10(numb/xx(1)) + kdec)
          if (xinch.lt.-.001 .or. xinch.gt. .001+xlen) goto 3400
c  Draw next section of axis and its tick mark
          call plot(0.0, xinch,  idn)
          if (numb.eq.1 .or. xslg.gt. 6.*hgt .and. tentix.ge.0) then
            shrnk=min(1.6,real(numb+iskp10))
            call plot((1-2*right)*htix/shrnk, xinch, idn)
            ngry=ngry + 1
            grxy(ngrx+ngry)=xinch
          endif
          if (nuy.gt. 0) then
c  If number of decades .lt.2 draw integers next to their ticks
            if (numb.ne. 1) then
              if (logd.le. 2  .and. .02*xslg .gt. hgt)
     $        call letter(space, xinch-0.5*hr, hr, 0.0, no(numb))
c  Or write out notation 10**kdec below appropriate tick
            elseif (iskp10.eq. 0) then
              write(label, '(i4)') kdec
              call justy(s, hr, label)
              back=(1-right)*(1.5*wid10+s(3)-s(1))-0.5*right*wid10
              backy=max(backy, 1.5*wid10+s(3)-s(1))
              call letter(wid10-back-s(1), xinch, hr, 0.0, label)
              call letter(-back, xinch-0.5*hgt, hgt, 0.0, no(10))
            endif
          endif
c  Move back onto axis with pen raised
          call plot(0.0, xinch, iup)
 3400   continue
 3500 continue
c  Plot last piece of axis 
      call plot(0.0,  xlen, idn)
      return
c
c  Interval less than a decade in length. Use standard axes routine
c  with logarithmic stretching option through /grdlog/
 4000 logik=1
      call axes(xlen, xx, 0.0,ixy)
      logik=0
      return
      end
c______________________________________________________________
      subroutine draw(x, y, ipen)
c$$$$ calls plot
c  Plug-in substitute for Calcomp  plot  giving dashed lines if fill(1),
c  a variable in common /kink/, is positive and  plot  if it is not.
c  fill(1)  is length of nonblank segments of dashed lines,
c  fill(2)  that of blank segments.  If  ipen=1,  initialize routine.
      common /kink/ fill(2),xo,yo,snext,ink
c
c  Check if dashed-line feature is needed or not
      ipen1=ipen
      if (fill(1).le. 0.0 .or. fill(2) .le. 0.0) goto 1250
      goto (1100, 1500, 1200), ipen
c  Initialize dashed line parameters
 1100 snext=0.0
      ink=3
      return
c  Move with pen lifted.  Leave dashed parameters alone
 1200 xo=x
      yo=y
 1250 if (ipen.ne.1) call plot(x, y, ipen1)
      return
c  Draw a section of dashed line.  s  is length of current segment
 1500 s=sqrt((x-xo)**2 + (y-yo)**2)
      if (s.eq. 0.0) return
      nseg=3.0 + 2.0*s/(fill(1) + fill(2))
      do 1600 l=1, nseg
        r=min(1.0, snext/s)
        ipen1=ink
        call plot(xo + r*(x-xo), yo + r*(y-yo), ipen1)
        if (s .lt. snext) goto 1610
        ink=5 - ink
        snext=fill(ink-1) + snext
 1600 continue
 1610 snext=snext - s
      xo=x
      yo=y
      return
      end
c______________________________________________________________
      subroutine ask
c$$$$ calls decode dfault
c  Reads a line from input device, splits off 1st 4 chars into
c  code , searches for beginning of next string as signaled by end
c  of blanks, then returns string in  line.
c  Up to 10 numbers are sought in string  line  which are put into
c  array  xdata.  The number found is in  nfound, while an error is
c  signaled in  nerr.  Results are returned in common /inform/.
      character*116 line,image, code*4
      common /inout/ in,iout,idisk
      common /inform/ nchar,nfound,nerr,xdata(10)
      common /char1/ code,line,image
      data noco/0/, init/1/
c
c  Read default commands, then go to command line
 1000 if (init.eq. 1) call dfault(init, code, image)
      if (init.ne. 1) read (*, '(a4, a116)', end=2000) code,image
      if (code(1:1).eq.' ' .or. code(1:1) .eq. '%') goto 1000
c  Take care of % comments
c ------ command  nocomment ------
      if (code .eq. 'noco')  then
        noco=1111
        goto 1000
      endif
      icom=index(image, '%')
      if (icom .gt. noco) image(icom:116)=' '
c
      ibegin=0
c  Modification to make upper-case letters in commands into lower case
c  (but not those in strings) - D. Agnew Nov 85
      icharA=ichar('A')
      icharZ=ichar('Z')
      ichdif=ichar('a') - icharA
      do 1050 n=1,4
        i = ichar(code(n:n))
        if(i.ge.icharA.and.i.le.icharZ) code(n:n) = char(i+ichdif)
 1050 continue
      do 1100 n=1,116
        if (image(n:n) .eq. ' ') ibegin=1
        if (image(n:n).ne.' ' .and. ibegin.eq.1) goto 1150
 1100 continue
      line=' '
      nfound=0
      nchar=0
      goto 1400
 1150 nchar=116-n+1
      do 1200 i=1,nchar
        k=nchar-i+1
 1200 if (image(k:k) .ne. ' ') goto 1300
 1300 continue
c  Put extra blank at end of line
      k=min(k+1, nchar)
      line=image(n:k)
      nchar=k - n + 1
      call decode(line(1:nchar), xdata, 10, nfound, nuff)
 1400 nerr = max(0, -nfound)
      nfound=max(0,  nfound)
      return
c  Eof is interpreted as 'stop' command
 2000 code='stop'
      return
      end
c______________________________________________________________
      subroutine decode(char, values, nwant, nfound, nuff)
c$$$$ calls no other routines
c  Evaluates numbers in string  char.  nwant  numbers are expected,
c  nfound  are actually found in character string.  When successful
c  char(1:nuff) is string containing numbers returned.  If an
c  error is discovered  nfound=-n,  where  n  is number of numbers
c  properly decoded.
      dimension values(*)
      character*(*) char, local*40
c
      kn=len(char)
      k1=1
c  Up to  nwant  numbers are sought
      do 1800 nval=1, nwant
        do 1100 k=k1, kn
          if (char(k:k) .ne. ' ') goto 1200
 1100   continue
        nfound=nval-1
        return
 1200   do 1300 l=k, kn
          if (char(l:l).eq. ',' .or. char(l:l) .eq. ' ') goto 1500
 1300   continue
 1500   local=char(k:l-1)
        read (local, '(f40.0)', err=1900) values(nval)
        k1=l+1
 1800 continue
      nval=-nwant
      nuff=l
 1900 nfound=-nval
      return
      end
c______________________________________________________________
      subroutine revers(n, x, xl, irev)
c$$$$ calls no other routines
c  Reverses sign of all arguments, except n
      dimension x(*),xl(2)
c
      do 1100 i=1, n
        x(i)=-x(i)
 1100 continue
      xl(1)=-xl(1)
      xl(2)=-xl(2)
      irev=-irev
      return
      end
c______________________________________________________________
      subroutine minmax(n, x, xl)
c$$$$ calls no other routines
c  Finds minimum and maximum of an array.  Returns  1  if n.le.0
      dimension x(*),xl(2)
c
      if (n.le. 0) x(1)=1.0
      xmin=x(1)
      xmax=x(1)
      do 1000 i=1, n
        xmin=min(xmin,x(i))
        xmax=max(xmax,x(i))
 1000 continue
      xl(1)=xmin
      xl(2)=xmax
      return
      end
c______________________________________________________________
      subroutine sort2(n, x, y)
c$$$$ calls no other routines
c  Sorts  (x,y) pairs numerically on x by the combsort, a souped 
c  up version of bubble sort (Lacey & Box, Byte 16, p315, 1991)
c  Almost as fast as quicksort.
      dimension x(*),y(*)
c
      ngap=n
 1000 ngap=max(int(ngap/1.3), 1)
      if (ngap.eq.9 .or. ngap.eq.10) ngap=11
      isw=0
      do 1500 i=1, n-ngap
        j=i + ngap
        if (x(i) .le. x(j)) goto 1500
        temp=x(i)
        x(i)=x(j)
        x(j)=temp
        temp=y(i)
        y(i)=y(j)
        y(j)=temp
        isw=1
 1500 continue
      if (isw.eq.1 .or. ngap .gt. 1) goto 1000
      return
      end
c______________________________________________________________________
      subroutine logit(iway, n, x)
c$$$$ calls no other routines
c  Creates log10 of an array or restores a previously logged one
      dimension x(*)
c
      do 1100 i=1, n
        if (iway.gt.0) x(i)=log10(x(i))
        if (iway.lt.0) x(i)=10.0**x(i)
 1100 continue
      return
      end
c______________________________________________________________
      subroutine nicer(xin, xtin, xout, xtick)
c$$$$ calls no other routines
c  Routine for scaling intervals and providing tick marks for axes.
c  Between 7 and 15 ticks are made, which is suitable for 10in axes.
c    Input parameters
c  xin(1),xin(2)  extremes of variable  x  in its own units.
c  xtin  suggested interval for ticks; may be over-ruled if there
c    are too may or too few ticks on the interval.
c    Output parameters
c  xout(1),xout(2)  adjusted extremes, made to be round numbers.
c    The new interval always covers old one.
c  xtick  distance between tick marks in  x  units (not inches).  If
c    computed by  nicer, this number is always a round number.
      dimension  divs(4),xin(2),xout(2)
      data e/1.0e-7/, divs/0.1, 0.2, 0.5, 1.0/
c
      xout(1)=xin(1)
      xout(2)=xin(2)
      if (xout(2).eq.xout(1)) xout(2)=xout(2) + abs(xout(2)) + 1.0
      plus=1000.204 + log10(xout(2)-xout(1))
      index=1.4969 + 2.857*mod(plus,1.0)
      units=divs(index)*10.0**(int(plus)-1000)
      npanel=(xin(2) - xin(1))/max(xtin, 0.001*units)
      if (2 .le. npanel .and. npanel .le. 101) units=xtin
      bias=(1.+e)*units*aint(1.+max(abs(xout(1)),abs(xout(2)))/units)
      xout(1)=xout(1) - mod(xout(1)+bias,units)
      xout(2)=xout(2) - mod(xout(2)-bias,units)
      if (abs(xout(1)/units) .le. .01) xout(1)=0.0
      if (abs(xout(2)/units) .le. .01) xout(2)=0.0
      xtick=units
      return
      end
c______________________________________________________________
      subroutine spline(n, x, u, s, a)
c$$$$ calls no other routines
c  Finds array  s  for spline interpolator  eval.
c  n   number of data points supplied.
c  x  array of x-coords where function is sampled.  xx(1),xx(2),...
c     must be a strictly increasing sequence.
c  u  array containing sample values that are to be interpolated.
c  s  output array of 2nd derivative at sample points.
c  a  working space array of dimension at least  n.
c  A parabola is fitted through 1st and last 3 points to find
c  the slopes.
c  If 3 points are given, a parabola is fitted; for 2 a straight line.
      common /startx/ istart
      dimension x(*),u(*),s(*),a(*)
c
      q(u1,x1,u2,x2)=(u1/x1**2-u2/x2**2)/(1.0/x1-1.0/x2)
c
      istart=1
      s(1)=0.0
      s(2)=0.0
      if (n.eq. 2) return
      q1=q(u(2)-u(1),x(2)-x(1),u(3)-u(1),x(3)-x(1))
      qn=q(u(n-1)-u(n),x(n-1)-x(n),u(n-2)-u(n),x(n-2)-x(n))
c  Too short for cubic spline - fit parabola for n=3, straight
      s(1)=(qn - q1)/(x(3) - x(1))
      s(2)=s(1)
      s(3)=s(1)
      if (n.eq. 3) return
 1000 s(1)=6.0*((u(2)-u(1))/(x(2)-x(1)) - q1)
      n1= n - 1
      do 2000 i=2,n1
        s(i)= (u(i-1)/(x(i)-x(i-1)) - u(i)*(1.0/(x(i)-x(i-1))+
     $  1.0/(x(i+1)-x(i))) + u(i+1)/(x(i+1)-x(i)))*6.0
 2000 continue
      s(n)=6.0*(qn + (u(n1)-u(n))/(x(n)-x(n1)))
      a(1)=2.0*(x(2)-x(1))
      a(2)=1.5*(x(2)-x(1)) + 2.0*(x(3)-x(2))
      s(2)=s(2) - 0.5*s(1)
      do 3000 i=3,n1
        c=(x(i)-x(i-1))/a(i-1)
        a(i)=2.0*(x(i+1)-x(i-1)) - c*(x(i)-x(i-1))
        s(i)=s(i) - c*s(i-1)
 3000 continue
      c=(x(n)-x(n1))/a(n1)
      a(n)=(2.0-c)*(x(n)-x(n1))
      s(n)=s(n) - c*s(n1)
c  Back substitute
      s(n)= s(n)/a(n)
      do 4000 j=1,n1
        i=n-j
        s(i) =(s(i) - (x(i+1)-x(i))*s(i+1))/a(i)
 4000 continue
      return
      end
c______________________________________________________________
      function eval(y, nn, x, u, s)
c$$$$ calls no other routines
c  Performs cubic spline interpolation of a function sampled unequally
c  in  x.  The routine spline  should be called to set up array s
c  y  coordinate at which function value is desired.
c  nn  number of samples of original function.
c  x  array containing sample coordinates. sequence x(1),x(2).....x(nn)
c     must be strictly increasing.
c  u  array containing samples of function at coords x(1),x(2)...
c  s  array containing 2nd derivatives at sample points.  Found by
c     routine  spline, which must be called once before interpolation.
c  If  y  falls outside range(x(1),x(nn))  value at nearest endpoint
c  of series is used.
      common /startx/ istart
      dimension x(*),u(*),s(*)
c
c  Out of range.  Substitute end value
      if (y .le. x(1))  then
        eval=u(1)
        return
      elseif (y .ge. x(nn)) then
        eval=u(nn)
        return
      endif
c  Locate interval (x(k1),x(k))  containing y
      if (y-x(istart)) 1200,1000,1000
c  Scan up x array
 1000 do 1100 k=istart,nn
        if (x(k).gt.y) goto 1150
 1100 continue
 1150 k1=k-1
      goto 1500
c  Scan downwards in x array
 1200 do 1300 k=1,istart
        k1=istart-k
        if (x(k1).le.y) goto 1350
 1300 continue
 1350 k=k1+1
 1500 istart=k1
c  Evaluate interpolate
      dy=x(k) - y
      dy1=y - x(k1)
      dk=x(k) - x(k1)
      ff1=s(k1)*dy*dy*dy
      ff2=s(k)*dy1*dy1*dy1
      f1=(ff1 + ff2)/(6.0*dk)
      f2=dy1*((u(k)/dk) - (s(k)*dk)/6.0)
      f3=dy*((u(k1)/dk) - (s(k1)*dk)/6.0)
      eval=f1 + f2 + f3
      return
      end
c______________________________________________________________
      blockdata blockb
c  Initialization of quantities in common blocks throughout plotxy
      character*64 name,pfile, note*80,tnote*80,morsel*16, ifmt*116
      parameter (maxnot=51, maxsel=600, not9 = 9*maxnot, maxknt=500)
      common /xyaxes/ nux,nuy,nax,nay,right,top,ixe,iye
      common /inout/ in,iout,idisk
      common /table/ kount(20,maxknt),ncount,ioff
      common /pmodes/ iplot,logxy,nch,ndrawn,jtrue
      common /char2/ pfile /nchr2/ npfile
      common /char3/ name,ifmt /nchr3/ nname
      common /paramr/ mode,x0,delx,ifopen,ifile,dash(2),laffin,affxy(4)
      common /paramp/ xlim(4),ylim(4),nxlab,nylab,ntit,hxlab,hylab,htit,
     $   height,angle,lw,iframe,kolab(3)
      common /notes/ nnotes,xyh(9,maxnot),leng(maxnot),kolnot(maxnot),
     $   jot1,xynote(2,maxsel),kolmrs(maxsel),hmrs(maxsel)
      common /char5/ note(maxnot),tnote,morsel(maxsel)
      common /backs/ backy,ht,hgt,htix
      common /grdlog/ logik,xslg,ngrx,ngry,grxy(200)
      common /penops/ iup,idn,mvor
      common /kink/ fill(2),xo,yo,snext,ink
      common /lnprop/ kolor,linfat
      common /xylim/ ilim, kbkgrd
c
c  common /xyaxes/ nux,nuy,nax,nay - numbers and x & y axes
      data nux,nuy/1,1/, nax,nay/1,1/, right,top/0,0/, ixe,iye/0,0/
c  common /inout/ -  terminal input, output unit numbers
      data in,iout/5,6/, idisk/7/
c  common /table/ - pointer and counter for data series
      data ioff/1/, ncount/0/
c  common /pmodes/char2/- pfile=default plotfile name
      data iplot/0/, logxy/0/, pfile(1:6)/'mypost'/, npfile/6/,ndrawn/0/
      data jtrue/0/
c  common /paramr/ - name=default data filename, ifmt=default format
c  mode xo delx=default mode params  dash=default dash params
      data name(1:7)/'* '/, ifmt(1:2)/'* '/, nname/6/,
     $ mode/2/, x0/1.0/, delx/1.0/,
     $ ifopen/0/, ifile/7/, dash/0.0, .07/,
     $ laffin/0/, affxy/1.0,0.0,1.0,0.0/
c  common /paramp/
      data xlim/-4,0,0,0/, ylim/-4,0,0,0/,   nxlab,nylab,ntit/3*0/,
     $ angle/0.0/, lw/2/, iframe/0/, hxlab,hylab/2*0.0/
     $ kolab/3*1/
c  common /notes/
      data nnotes/0/, kolnot/maxnot*1/, jot1/0/, xyh/not9*0/
c  common /backs/
      data ht/0.1/, htix/0.1/
c  common /grdlog/
      data logik/0/
c  common /penops/
      data iup,idn,mvor/3,2,-3/
c  common /kink/
      data fill/0.0, 0.1/, ink/3/
c  common /lnprop/
      data kolor /6001/, linfat/0/
c  common /xylim/ 
      data kbkgrd/-1/
c
      end
c______________________________________________________________
      subroutine expand(logo, alim, range, logx)
c$$$$ calls no other routines
c  logo    is  0  for linear scales  1  for log scales.
c  alim(1) axis length in inches.
c  alim(2-3)    user supplied plot limits, which may be inconsistent.
c  range  on input min-max of data, on output range to be used.
c  logx    revised specification for log scales - different from  logo
c          if lower limit or least data value is nonpositive.
      dimension alim(3),range(2)
      common /backs/ backy,ht,hgt,htix
c
c  Upper, lower limits (almost) equal: expand by+-10%
      if (range(2)-range(1).le.3e-6*abs(range(2)+range(1))) then
        range(2)=range(2) + 0.1*abs(range(2))
        range(1)=range(1) - 0.1*abs(range(1))
        if (range(1) .eq. range(2)) range(2)=1.0
      endif
      if (alim(2).ge.alim(3)) goto 1100
      if ((alim(2).le.0.0.or.range(1).le.0.0) .and. logo.eq.1)goto 1200
      logx=logo
      range(1)=alim(2)
      range(2)=alim(3)
      return
c
c  Add margins
 1100 xmarg=max(0.05, 2.5*htix)/abs(alim(1))
      if (logo.eq.0 .or. range(1).le.0.0) goto 1200
c  Log axes expanded
      logx=1
      rxl=(range(2)/range(1))**xmarg
      range(1)=range(1)/rxl
      range(2)=range(2)*rxl
      return
c  Linear axis expanded
 1200 logx=0
      dxl=xmarg*(range(2) - range(1))
      range(1)=range(1) - dxl
      range(2)=range(2) + dxl
      return
      end
c______________________________________________________________
      subroutine akima(n, x,f, df)
c$$$$ calls nothing
c  Given the array of samples of a function f and sample points x,
c  assumed to be montone, generates slopes for an interpolating rule
c  according to Akima's algorithm (Lancaster and Salkauskas,
c  Curve and surface fitting, 1986 Academic Press, p 82).
c
      dimension x(n),f(n),df(n),S(4)
c
      q(u1,x1,u2,x2)=(u1/x1**2-u2/x2**2)/(1.0/x1-1.0/x2)
c
ce
      S(1)=(f(2) - f(1))/(x(2) - x(1))
      S(2)=S(1)
      S(3)=S(1)
      S(4)=(f(3) - f(2))/(x(3) - x(2))
      Sn=(f(n) - f(n-1))/(x(n) - x(n-1))
      eps=1.0e-6*abs(x(n) - x(1))
      do 1200 i=1, n
        D1=abs(S(2) - S(1))
        D2=abs(S(4) - S(3))
        df(i)=(D2*S(2) + D1*S(3))/(D1 + D2 + eps)
c
        S(1)=S(2)
        S(2)=S(3)
        S(3)=S(4)
        if (i+3 .le. n) then
          S(4)=(f(i+3) - f(i+2))/(x(i+3)-x(i+2))
        else
          S(4)=Sn
        endif
 1200 continue
c
      if (n.eq. 2) return
c  If 3 or more points use gradient from a parabola for 1st & last df
      df(1)=q(f(2)-f(1),x(2)-x(1),f(3)-f(1),x(3)-x(1))
      df(n)=q(f(n-1)-f(n),x(n-1)-x(n),f(n-2)-f(n),x(n-2)-x(n))
      return
      end
c____________________________________________________________________
      function evlak(y, n,x,f,df)
c$$$$ calls nothing
c  Given a montone increasing array of x values (Note: routine does not
c  check this), samples of values of the function f and 1st derivative
c  df at the xs, interpolates by cubic polynomial.  The array df can be
c  found by calling subroutine akima.
c
c  If y falls outside the interval, the polynomial on the
c  nearest interval is used to extrapolate.
      dimension x(n), f(n), df(n)
c 
c  Search for proper interval initiated at previous call if possible.
      save init
      data init/1/
c
c  Locate sample interval containing y:  after y lies in [x(init),
c  x(init+1)), unless y lies outside [x(1), x(n)] when the interval
c  is the intervals containing the apprpriate end point.
ce
      init=min(init, n)
      if (y .gt. x(init)) then
         do 1100 k=init, n
           if (x(k) .gt. y) then
             init=k-1
             goto  1300
           endif
 1100    continue
         init=n-1
       else
         do 1200 k=init, 1, -1
           if (x(k) .le. y)  then
             init=k
             goto 1300
           endif
 1200   continue
        init=1
      endif
 1300 dx=x(init+1) - x(init)
c
c  Evaluate the cubic interpolator
      t=(y - x(init))/dx
      s=1.0 - t
      evlak=s**2*((1.0 + 2.0*t)*f(init) + t*dx*df(init)) +
     $      t**2*((1.0 + 2.0*s)*f(init+1) - s*dx*df(init+1))
      return
      end
c____________________________________________________________________
****************************************************************
* POSTSCRIPT DRIVERS FOR THE STANDARD PLOT SUBROUTINES
****************************************************************
      subroutine plot(x, y, ipen)
c$$$$ calls no other routines
      common /post/ boxx,boxy,land,infill
      common /outsid/ box(4),xorig,yorig
      save moves
      data moves/0/
c  PostScript output generator for the old CalComp line-drawing routine
c  Writes to unit 11; abbreviations  k, l, m, n, f  must be defined
ce
      ix=1000.0*x
      iy=1000.0*y
c
c  Extend current polygon; if 1000 reached, stroke it and start over
      if (ipen .eq. 2) then
c  Keep record of maximum excursion for Encapsulation
        box(1)=min(box(1), x+xorig)
        box(2)=min(box(2), y+yorig)
        box(3)=max(box(3), x+xorig)
        box(4)=max(box(4), y+yorig)
        if (moves .le. 1000) then
          write(11,*) ix,iy,' l'
          moves=moves + 1
        else
          moves=0
          write(11,*) ix,iy,' n'
        endif
c  Stroke and terminate current ploygon; start a new one at (x,y)
      elseif (ipen.eq. 3) then
        if (moves .gt. 0) then
c  Fill the polygon if infill set to 1; otherwise just stroke
          if (infill .eq. 1) write(11,*) ix,iy,' f'
          if (infill .eq. 0) write(11,*) ix,iy,' k'
          moves=0
        else
          write(11,*) ix,iy,' m'
        endif
      elseif (ipen .lt.  0) then
        write(11,*) ix,iy,' translate',
     $  0 ,0,  ' moveto'
        xorig=xorig + 0.001*ix
        yorig=yorig + 0.001*iy
      elseif (ipen .eq. 0) then
c  If ipen = 0, eject a page;  insert signal for psview too.
        write(11,'(2i8,a/(a))') ix,iy,' moveto',
     $  'currentpoint translate',
     $  'showpage',
     $  '%%Page: fresh page started ---------------------------'
c  If ipen = 4 write the fill command
      elseif (ipen .eq. 4) then 
         write (11,*) 'currentpoint f'
      else
        print *,'Unintelligible pen command in plot ignored'
      endif
      return
      end
c_______________________________________________________________
      subroutine plopen(k, name)
c$$$$ calls no other routines
c  When k > 0 opens unit 11 for PostScript and writes the short prolog.
c  Prolog includes landscape rotation if land > 0 in /caps/
c  When k < 0 closes the file
c
      common /post/ boxx,boxy,land,infill
      common /outsid/ box(4),xorig,yorig
      character*(*) name
c
c  When k < 0 closes the file: flush polygon buffer and display
c  info for encapsulating written after showpage
      if (k .lt. 0) then
         write(11,'(a)') 'currentpoint n','showpage'
         write(11,'(a/a,4f9.1)') '%!PS-Adobe-2.0 EPSF-1.2',
     $   '%%BoundingBox:',(72*box(j),j=1,4)
         close(unit=11)
         do 1100 j=1, 4
           box(j)=(2.5-j)*1.0e6
 1100   continue
        xorig=0.0
        yorig=0.0
c
c  When k > 0 opens unit 11 for PostScript; writes prolog
      else
         if (name .eq. 'myplot') name='mypost'
         open(unit=11, file=name)
         print '(/a/2a/a)',
     $   ' --------------------------------------',
     $   ' PostScript file written to: ',name,
     $   ' ========== ---------------------------'
c
c  Write PostScript prolog containing PS procedures
         write(11,'(a)')'%!PS PostScript file',
     $   '% Generated by plotxy',
     $   '/k {currentpoint stroke moveto newpath moveto} def',
     $   '/l {lineto} def',
     $   '/m {newpath moveto} def',
     $   '/n {lineto currentpoint stroke moveto} def',
     $   '/f {currentpoint closepath fill moveto newpath moveto} def',
     $   '0.072 0.072 scale % Coords are 1/1000 th inch'
         if (land .gt. 0) write(11,'(a)')
     $  '%8500 0 translate 90 rotate    % anticlockwise',
     $  '-2500 11000 translate -90 rotate % clockwise'
c  Set up a background color
         write(11,'(a)') '2 setlinejoin 4 setlinewidth 1 setlinecap'
      endif
      return
      end
c_______________________________________________________________
      subroutine newpn(ipen)
c$$$$ calls no other routines
c  Uses Postscript command to reset the color and line width.
c  Color table held in array below as hsb values
      parameter (ncol=48)
      common /hues/ table(3,ncol)
      save lastpn
      data lastpn/-9/
c
c  0 or 1: black; 2: red; 3: blue;   4: green; 5: brown;
c  6: orange; 7: yellow;  8: purple; 9: gray; 10: white.
c
c  Don't bother to change color if it doesn't change
      if (ipen .eq. lastpn) return
c  Flush current polygon buffer with previous parameters
      write(11,*) 'currentpoint k'
c  Ipen/1000 is line width in 1/1000 inches
      iwide=ipen/1000
      if (iwide.le. 0) iwide=4
c  By convention ipen=0, 1 => black
      ip=min(max(mod(ipen, 1000), 1), ncol)
      write(11,'(i5,a)') iwide,' setlinewidth'
      write(11,'(3f8.3,a)') (table(i,ip),i=1,3),' sethsbcolor'
      lastpn=ipen
      return
      end
c_______________________________________________________________
      subroutine remark(text)
c$$$$ calls no other routines
c  Inserts the remark text into the PostScript file
      character*(*) text
c
      write(11,'(2a)')'%',text
      return
      end
c_______________________________________________________________
      blockdata encaps
c  Sets a initial page size for encapsulated PS variables
      common /outsid/ box(4),xorig,yorig
      common /post/ boxx,boxy,land,infill
      data boxx,boxy/ 570.0,756.0/, land/0/, infill/0/
      data  box/1e6,1e6,-1e6,-1e6/
      data xorig,yorig/0.0,0.0/
      end
c_______________________________________________________________
      blockdata shades
c  Sets the colors in common /hues/
c 0 or 1: black; 2: red;   3: blue; 4: green; 5: brown
c    6: orange;  7: yellow;  8: purple;  9:gray; 10: white
c   11: light gray; 12-22: dark versions; 23-33 light versions
      parameter (ncol=48)
      common /hues/ table(3,ncol)
      data ((table(i,j),i=1,3), j=1, ncol)/
     $0,0,0,
     $  0,1,1,
     $  0.667,1,1,
     $  0.4,1,1,
     $  0.1,1,.7,
     $  0.06,1,1,
     $  0.15,1,1,
     $  0.833,1,1,
     $  0,0,0.6,
     $  0,0,1,
     $  0,0,0.8,
     $0,0,0,
     $  0,1,0.5,
     $  0.667,1,0.5,
     $  0.4,1,.45,
     $  0.1,1,.35,
     $  0.04,1,0.7,
     $  0.15,1,0.5,
     $  0.833,1,0.5,
     $  0,0,0.4,
     $  0,0,0.90,
     $  0,0,0.90,
     $0,0,0,
     $  0,0.4,1,
     $  0.667,0.4,1,
     $  0.35,0.5,1,
     $  0.1,0.5,.7,
     $  0.08,0.7,1,
     $  0.15,0.5,1,
     $  0.833,0.5,1,
     $  0,0,0.7,
     $  0,0,1,48*0/
      end
c_______________________________________________________________
      subroutine letter(x, y, height, theta, text)
c$$$$ calls chrcod plot glyph
c  Generates text and symbol in a variety of fonts to be plotted
c  as if with a pen-plotter using the Calcomp routine 'plot':
c    1) four Hershey letter fonts--simplex,complex,italic,duplex--
c       in upper and lower case Roman
c    2) two Hershey letter fonts--simplex and complex--in
c       upper and lower case Greek
c    3) 47 special mathematical symbols, eg. integral sign,del
c    4) super- and subscripting is provided
c
c    Change of font: enclose the name of the font in backslashes, eg.
c  \simplex\.  Three letters suffice to specify the font.  Simplex is
c  the default font on the initial call.  A font remains in effect
c  until explicitly changed.  Math font \$$\ forces italic letters,
c  retains previous font for other characters.
c    Super- or subscripts:  enclose the expression in curly brackets
c  and precede it with \sub or \sup; eg.  x\sup{2}.  Super- and 
c  subscript characters are plotted smaller.
c    Greek letters are drawn by enclosing the English name of the
c  letter in backslashes, e.g. \alpha\.  The case of the first letter
c  determines the case of the Greek letter.
c    The special graphical symbols (box, triangle, etc) can be included
c  text by enclosing the appropriate symbol integer+2000 in backslashes.
c  See subroutine glyph for the special symbol integers.
c    Any regular symbol may be drawn by enclosing the symbol number+1000
c  in backslashes.  This is the only way to call some symbols, notably
c  special mathematical symbols.
c  These regular symbols have the following integer codes:
c   1-26   upper case Roman simplex
c  27-52   lower case Roman simplex
c  53-72   simplex numbers and symbols
c  73-96   upper case Greek simplex
c  97-120  lower case Greek simplex
c  121-146 upper case Roman complex
c  147-172 lower case Roman complex
c  173-192 complex numbers and symbols
c  193-216 upper case Greek complex
c  217-240 lower case Greek complex
c  241-266 upper case Roman italic
c  267-292 lower case Roman italic
c  293-312 italic numbers and symbols
c  313-338 upper case Roman duplex
c  339-364 lower case Roman duplex
c  365-384 duplex numbers and symbols
c  385-433 special mathematical symbols
c
c  Symbol parameters taken from N.M.Wolcott, Fortran IV enhanced
c  Character Graphics, NBS
c
c  A. Chave IGPP/UCSD Aug 1981, improved by R. L. Parker
c
c  x, y   are coordinates in inches from current origin to 
c         lower left corner of 1st character to be plotted.  If either
c         is set to 999.0 the saved next character position is used.
c  height is character height in inches
c  text   is a character string to be plotted
c  theta  is anticlockwise angle from x-axis in degrees
c
c  Programmed in FORTRAN-77
c
      character*(*) text
      integer istart(433),isstar(22),symbcd(4711),ssymbc(128)
      real width(433),supsub(2),raise(20)
      common /ofset/ ioff,just1,just2,math
      common /ajust/ nchr,ichr(132)
      common /ialph/ width,symbcd,istart,ssymbc,isstar
      common /crown/ yx(433)
      save xo,yo
      data rad/.017453292/, xo,yo/0,0/
      data factor/0.75/,  supsub/0.50,-0.50/,  iup/3/
c  ichr(j): symbol number of the jth symbol or a code: 
c  1000 space; 1001 begin super-scripting; 1002 begin sub-scripting;
c  1003 end super/sub-scripting; 1004 back-space; 
c  1005 put hat on previous character; 1007 tilde; 1008 bar.
c  istart(ichr(j)) contains the address in symbol of the jth
c  character.  symbcd contains pen instructions stored in a
c  special format.  isstar and ssymbc contain addresses and pen
c  instructions for special centered symbols.  width contains
c  widths of the characters.
c
c  ixtrct gets nbits from iword starting at the nstart bit from the
c  right in an array of 32-bit integers.
      ixtrct(nstart,nbits,iword)=mod(iword/(2**(nstart-nbits)),
     $ 2**nbits)+((1-sign(1,iword))/2)* 
     $(2**nbits - min(1,mod(-iword, 2**(nstart-nbits))))
c
ce
      ntext=len(text)
      yoff=0.0
      si=sin(rad*theta)
      co=cos(rad*theta)
      high=height
      scale=high/21.0
      if (scale.eq.0.) return
      if (x.ge.999.0) then
        xi=xo
      else
        xi=x
      endif
      if (y.ge.999.0) then
        yi=yo
      else
        yi=y
      endif
c  Plot a character string.
c  First find pointer array  ichr  containing the starts of characters-
c  but only if  just1 and just2  are not 1, when  ichr is assumed
c  correctly transmitted through common /ajust/.
      if (just1.ne.1 .or. just2.ne.1) 
     $ call chrcod(text, ntext, ichr, nchr)
      just2=2
      oldwid=0.0
      ik=1
      l=1
      rscale=scale
c  Plot each character
      do 100 i=1,nchr
         ic=ichr(i)
c  ic < 0:  Change the character height
         if (ic.le.0) then
            rscale=-0.01*rscale*ic/high
            high  =-0.01*ic
c  ic = 1000: Plot a space
         elseif (ic.eq.1000)  then
           xi=xi + 20.*rscale*co
           yi=yi + 20.*rscale*si
           xo=xi
           yo=yi
           call plot(xi, yi, iup)
c  2000 <= ic <= 2021: Special graphics symbol code 2000-2021
          elseif (ic.ge.2000 .and. ic.le.2021) then
            hgt=20.0*rscale
            xo=xi + hgt*co
            yo=yi + hgt*si
            call glyph(0.5*(xi+xo-hgt*si), 0.5*(yi+yo+hgt*co),
     $      0.9*hgt, ic -2000)
            xi=xo
            yi=yo
c  ic = 1001 or 1002: Begin super-scripting or sub-scripting
         elseif (ic.eq.1001 .or. ic.eq.1002) then
            raise(l)=supsub(ic-1000)*high*rscale/scale
            rscale=factor*rscale
            yoff=raise(l)+yoff
            l=l+1
c  ic =1003: End super/sub-scripting
         elseif (ic.eq.1003) then
           rscale=rscale/factor
           l=l-1
           yoff=yoff-raise(l)
c  ic = 1004:  Backspace - use width of previous letter in oldwid.
         elseif (ic.eq.1004) then
           xi=xi - co*oldwid
           yi=yi - si*oldwid
           xo=xi
           yo=yi
c  ic = 1005: Decorate previous character with a hat
c  ic = 1007: Decorate previous character with a tilde
c  ic = 1008: Decorate previous character with a bar
         elseif (ic.eq.1005 .or. ic.eq.1007 .or. ic.eq.1008) then
           if (ic .eq. 1005) is=istart(408)
           if (ic .eq. 1007) is=istart(424)
           if (ic .eq. 1008) is=istart( 64)
           yyoff=yoff + rscale*(int(yx(ik)) - 2.5)
           dxhat=oldwid - rscale*(mod(100*yx(ik),100.0)- 9.0)
           if (ic .eq. 1008) dxhat=1.2*dxhat - 0.2*oldwid
           ib=30
   60      ipen=ixtrct(ib,3,symbcd(is))
           if (ipen.eq.0) goto 100
           ix=ixtrct(ib-3,6,symbcd(is))
           iy=ixtrct(ib-9,6,symbcd(is))
           xx=(ix-10)*rscale - dxhat
           yy=(iy-11)*rscale + yyoff
           call plot(xi+co*xx-si*yy, yi+co*yy+si*xx, ipen)
           ib=45-ib
           if (ib.eq.30) is=is+1
           goto 60
c  ic = 1006: Space forward about 1/3 regular amount
         elseif (ic.eq.1006)  then
           xi=xi + 6.66*rscale*co
           yi=yi + 6.66*rscale*si
           xo=xi
           yo=yi
         else
c
c  ic = everything else: Plot a single symbol
           is=istart(ic)
           ik=ic
           ib=30
   70      ipen=ixtrct(ib,3,symbcd(is))
           if (ipen.eq.0) then
             xi=xi+co*rscale*width(ic)
             yi=yi+si*rscale*width(ic)
             xo=xi
             yo=yi
             oldwid=width(ic)*rscale
             goto 100
           endif
           ix=ixtrct(ib-3,6,symbcd(is))
           iy=ixtrct(ib-9,6,symbcd(is))
           xx=(ix-10)*rscale
           yy=(iy-11)*rscale+yoff
           call plot(xi+co*xx-si*yy, yi+co*yy+si*xx, ipen)
           ib=45-ib
           if (ib.eq.30) is=is+1
           goto 70
         endif
  100   continue
      return
      end
c_______________________________________________________________________
      subroutine chrcod(text, ntext, ichr, nchr)
c$$$$ calls nothing
c  Given text string in text, ntext characters returns ichr containing
c  nchr symbol numbers or codes (see comments in routine letter).
c  Change of font commands are decoded and executed internally
c
      common /ofset/ ioff,just1,just2,math
      character*1 bkslsh
      character*(*) text
      integer ichr(132),irlu(95),iilu(95),iglu(26)
c      data ioff/0/
c  irlu is a roman look-up for Roman characters arranged by
c  integer value for the ascii character set with an
c  offset to remove 31 nonprinting control characters.
c  irlu gives the symbol number or, if no symbol
c  exists, the code for space = 1000
      data irlu/1000,416, 428,411,72,418,419, 432,67,68,69,63,70,
     $          64,71,65,53,54,55,56,57,58,59,60,61,62,414,415,
     $          385,66,386,417,407,1,2,3,4,5,6,7,8,9,10,11,12,13,
     $          14,15,16,17,18,19,20,21,22,23,24,25,26,409,1000,
     $          410, 408, 433, 432,27,28,29,30,31,32,33,34,35,36,
     $          37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,
     $          405,427,406,424/
c  iilu is a look-up table for italic characters.  Identical to
c  irlu with 4 italic special symbols substituted for regular ones.
      data iilu/1000,422, 428,411,72,418,419, 432,67,68,69,63,70,
     $          64,71,65,53,54,55,56,57,58,59,60,61,62,420,421,
     $          385,66,386,423,407,1,2,3,4,5,6,7,8,9,10,11,12,13,
     $          14,15,16,17,18,19,20,21,22,23,24,25,26,409,1000,
     $          410,1000, 433, 432,27,28,29,30,31,32,33,34,35,36,
     $          37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,
     $          405,427,406,424/
c  iglu is Greek look-up table for characters arranged by the
c  integer value of their Roman expression with a=1, b=2, etc.
c  Ambiguous cases: 25 for epsilon or eta; 26 for omega or
c  omicron; 27 for phi,pi,or psi; and 28 for tau or theta.  Additional
c  letters must be checked in these cases. The value 50 is returned
c  for those Roman letters which have no corresponding Greek letter.
      data iglu/1,2,22,4,25,50,3,50,9,50,10,11,12,13,26,27,50,17,18,
     $             28,20,50,50,14,50,6/
c
c  Due to special nature of backslash in Unix, generate it from ASCII.
      bkslsh=char(92)
c
c Finds length of string with blanks trimmed from right end.
      do 10 n=ntext,1,-1
        if (text(n:n) .ne. ' ') goto 15
 10   continue
      nchr=0
      return
 15   nt=n
c  Scan text character by character
      k=1
      j=1
c  k is current address of character in text
c  j is index of next symbol code in ichr
   20 if (k.gt.n) then
        nchr=j-1
        return
      endif
      if (text(k:k).ne. bkslsh) then
c  Roman character or keyboard symbol
        if (text(k:k).eq.'}') then
c  Check for closing curly bracket-if found, return 1003
           ichr(j)=1003
           j=j+1
           k=k+1
           goto 20
         endif
c 
c  The intrinsic function ichar returns ASCII integer of character 
c  offset by nonprinting characters
        ic=ichar(text(k:k))-ichar(' ')+1
        joff=0
c  Force italic font on alphabetic chars when math >= 0.  Save old
c  offset in math
        if (math .ge.0 .and. (34 .le.ic .and. ic.le. 59 .or.
     $      66.le.ic .and. ic.le.91)) then
          math=ioff
          joff=1
          ioff=240
        endif
c  Nonprinting control character-error return
        if (ic.le.0) then
          ichr(j)=1000
c  Not italic font
        elseif (ioff.ne.240) then
           ichr(j)=irlu(ic)
        else
c  Italic font
          ichr(j)=iilu(ic)
        endif
c  Add offset for font if not a special symbol
        if (ichr(j).lt.385) ichr(j)=ichr(j)+ioff
        j=j+1
        k=k+1
        if (joff .eq. 1) ioff=math 
        goto 20
      else
c
c  Backslash found
         k=k+1
c
c  Check for sundry mathematical symbols, substitute 4-digit codes
c  Regular theta: 'rgth' 
         if (text(k:k+3) .eq. 'rgth') then
           text(k:k+3)='1404'
c  Regular phi:  'rgph'
         elseif (text(k:k+3) .eq. 'rgph') then
           text(k:k+3)='1431'
c  Infinity: 'infi'
         elseif (text(k:k+3) .eq. 'infi') then
           text(k:k+3)='1395'
c  Integral: 'inte'
         elseif (text(k:k+3) .eq. 'inte') then
           text(k:k+3)='1393'
c  Partial: 'part'
         elseif (text(k:k+3) .eq. 'part') then
           text(k:k+3)='1387'
c  Grad:  'grad'
         elseif (text(k:k+3) .eq. 'grad') then
           text(k:k+3)='1388'
c  Times: 'time' 
         elseif (text(k:k+3) .eq. 'time') then
           text(k:k+3)='1398'
c  Degrees: 'degr' 
         elseif (text(k:k+3) .eq. 'degr') then
           text(k:k+3)='1429'
c  Square-root: 'sqrt'
         elseif (text(k:k+3) .eq. 'sqrt') then
           text(k:k+3)='1402'
         endif
c
c
c  Check next four characters for 4-digit number
         read (text(k:k+3), '(i4)', err=50) number
c  Number found - valid numbers are
c         0<n<500  or  999<n<1433  or 1999<n<2022
c
c  3-digit number changes letter height
        if (number.gt.0 .and. number.lt.500) then
          ichr(j)=-number
c  Special graphics symbol for glyph
        elseif (number.gt.1999 .and. number.le.2021) then
           ichr(j)=number
c  Valid symbol code
         elseif (number.gt.999 .and. number.lt.1433) then
           ichr(j)=number - 1000
         else
c  Not recognized - plot a blank
           ichr(j)=1000
         endif
         j=j+1
c  Move beyond closing backslash -- ignore extra characters
        l=index(text(k:nt), bkslsh)
        if (l.eq.0) then
           k=nt+1
         else
            k=k+l
         endif
         goto 20
c
c  Not a number in the backslashes
   50  continue
c
c  Check for font change command
c  Simplex font
       if (text(k:k+2).eq.'SIM'.or.text(k:k+2).eq.'sim') then
         ioff=0
         math=-1
c  Complex font
       elseif (text(k:k+1).eq.'CO'.or.text(k:k+1).eq.'co') then
         ioff=120
         math=-1
c  Italic font
       elseif (text(k:k+1).eq.'IT'.or.text(k:k+1).eq.'it') then
         ioff=240
         math=-1
c  Duplex font
       elseif (text(k:k+1).eq.'DU'.or.text(k:k+1).eq.'du') then
         ioff=312
         math=-1
c  Math font => italics for letters, previous font otherwise
c  Toggles math state so that \$\ ... \$\ is math expression
       elseif (text(k:k).eq.'$') then
         if (math .eq. -1) then
           math=ioff
c  \$$\ forces math font, so don't revert
         elseif (text(k:k+1).ne. '$$') then
           ioff=math
           math=-1
         endif
c  Found the back-space code
       elseif (text(k:k+1).eq.'BS'.or.text(k:k+1).eq.'bs') then
         ichr(j)=1004
         j=j+1
         k=k+3
         goto 20
c  Check for super/sub-script command
       elseif (text(k:k+3).eq.'SUP{'.or. text(k:k+3).eq.'sup{') then
c  Begin superscripting
         ichr(j)=1001
         j=j+1
         k=k+4
         goto 20
       elseif (text(k:k+3).eq.'SUB{'.or. text(k:k+3).eq.'sub{') then
c  Begin subscripting
         ichr(j)=1002
         j=j+1
         k=k+4
         goto 20
c  Put a hat over the previous character
       elseif(text(k:k) .eq. '^') then
         ichr(j)=1005
         j=j+1
         k=k+2
         goto 20
c  Put a tilde over the previous character
       elseif(text(k:k) .eq. '~') then
         ichr(j)=1007
         j=j+1
         k=k+2
         goto 20
c  Put a bar over the previous character
       elseif(text(k:k) .eq. '-') then
         ichr(j)=1008
         j=j+1
         k=k+2
         goto 20
c  Space forward 1/3 of regular space 
       elseif(text(k:k) .eq. ' ') then
         ichr(j)=1006
         j=j+1
         k=k+2
         goto 20
       else
c  Greek character or invalid character
      ic=ichar(text(k:k))
      igoff=min(ioff, 120)
      if (ic.ge.ichar('A') .and. ic.le.ichar('Z')) then
c  Upper case
c  Greek character or invalid character
        igr=72
        ico=ichar('A')-1
      elseif (ic.ge.ichar('a') .and. ic.le.ichar('z')) then
c  Lower case
        igr=96
        ico=ichar('a')-1
      else
c  Not a letter-error return
        ichr(j)=1000
        j=j+1
        l=index(text(k:nt), bkslsh)
        if (l.eq.0) then
          k=nt+1
        else
          k=k+l
        endif
        goto 20
      endif
c  Look up the character
      ig=iglu(ic-ico)
      if (ig.lt.25) then
c  Unambiguous Greek letter
        ichr(j)=ig+igr+igoff
      elseif (ig.eq.25) then
c  Epsilon or eta
        ib=ichar(text(k+1:k+1))-ico
        if (ib.eq.16) then
c  Epsilon
          ichr(j)=5+igr+igoff
        elseif (ib.eq.20) then
c  Eta
          ichr(j)=7+igr+igoff
        else
c  Not a Greek character--error return
          ichr(j)=1000
        endif
      elseif (ig.eq.26) then
c  Omega or omicron
        ib=ichar(text(k+1:k+1))-ico
        if (ib.ne.13) then
c  Not a Greek character-error return
          ichr(j)=1000
        else
          ic=ichar(text(k+2:k+2))-ico
          if (ic.eq.5) then
c  Omega
            ichr(j)=24+igr+igoff
          elseif (ic.eq.9) then
c  Omicron
            ichr(j)=15+igr+igoff
          else
c  Not a Greek character-error return
            ichr(j)=1000
          endif
        endif
        elseif (ig.eq.27) then
c  Phi,pi, or psi
          ib=ichar(text(k+1:k+1))-ico
          if (ib.eq.8) then
c  Phi
            ichr(j)=21+igr+igoff
            elseif (ib.eq.9) then
c  Pi
              ichr(j)=16+igr+igoff
            elseif (ib.eq.19) then
c  Psi
              ichr(j)=23+igr+igoff
            else
c  Not a Greek character-error return
              ichr(j)=1000
            endif
          elseif (ig.eq.28) then
c  Tau or theta
            ib=ichar(text(k+1:k+1))-ico
          if (ib.eq.1) then
c  Tau
            ichr(j)=19+igr+igoff
          elseif (ib.eq.8) then
c  Theta
            ichr(j)=8+igr+igoff
          else
c  Not a Greek character-error return
            ichr(j)=1000
          endif
        else
c  Not a Greek character-error return
          ichr(j)=1000
        endif
          j=j+1
      endif
      l=index(text(k:nt), bkslsh)
      if (l.eq.0) then
        k=nt+1
      else
        k=k+l
      endif
      goto 20
      endif
      return
      end
c_______________________________________________________________________
      subroutine justy(s, height, text)
c$$$$ calls chrcod
c  Given the text string  text  with  ntext  characters,
c  height  high in inches,  this routine
c  gives 4 distances in inches, all from the left end of the string -
c  s(1)  to the left edge of the 1st nonblank character
c  s(2)  to the center of the the string, blanks removed from the ends
c  s(3)  to the right edge of the last nonblank character
c  s(4)  to the right edge of the last character of the string.
      character*(*) text
      dimension s(4),ipower(3)
      common /ialph/ width(433),sym1(4711),is1(433),sym2(128),is2(22)
      common /ofset/ ioff,just1,just2,math
      common /ajust/ nchr,ichr(132)
      data ipower/1,1,-1/,  factor/0.75/
c
      ntext=len(text)
      scale=height/21.0
      keep=math
      call chrcod(text, ntext, ichr, nchr)
      nchr=max(1, nchr)
      math=keep
c
c  Count leading blanks.
      do 1100 lead=1, nchr
        if (ichr(lead) .ne. 1000) goto 1110
 1100 continue
      lead=nchr
 1110 s(1)=20.0*scale*(lead-1)
      s(3)=s(1)
c
c  Sum the widths of the remaining text, recalling that trailing blanks
c  were lopped off by  chrcod.
      oldwid=0.0
      do 1200 i=lead, nchr
        l=ichr(i)
c  Change character height
        if (l .lt. 0) scale=-0.01*l/21.0
c  Regular character
        if (l.gt.0 .and. l.lt.1000) then
          oldwid=width(l)*scale
          s(3)=s(3) + oldwid
        endif
c  Glyph symbol or space
        if (l.eq.1000.or.l.ge.2000) s(3)=s(3) + 20.0*scale
c  Sub/super script size change
        if (l.ge.1001.and.l.le.1003) scale=scale*factor**ipower(l-1000)
c  Backspace
        if (l.eq.1004) s(3)=s(3) - oldwid
 1200 continue
c
c  Add on width of surplus trailing blanks.
      s(4)=s(3) + 20.0*scale*(ntext-nchr)
c
c  Find center of nonblank text.
      s(2)=(s(1) + s(3))/2.0
      just2=1
      return
      end
c_______________________________________________________________________
      subroutine glyph(x, y, height, nglyph)
c$$$$ calls plot
c  Draws centered symbols.
c  At coordinates  (x, y)  in inches measured from current origin,
c  draws a single centered symbol, height  height, and identity 
c  specified by  nglyph  as follows -
c  0 square,    1 triangle, 2 octagon,        3  diamond, 4  plus,
c  5 asterisk,  6 cross,    7 slashed square, 8 up-arrow, 9 hourglass,
c  10 campstool,11 hexagon, 12 Y,         13 |,       14 star of David
c  15 dot,      16 sm circ, 17 circle,    18 lg circ, 19 filled sm circ
c  20 filled sm square,     21 filled sm triangle.
c  Data for these glyphs come through common /ialph/.
      integer istart(433),isstar(22),symbcd(4711),ssymbc(128),map(22)
      common /ialph/ width(433),symbcd,istart,ssymbc,isstar
      data map/0,2,1,5,3,11,4,10,6,12,7,8,9,13,14,15,
     $16,17,18,19,20,21/
c  ixtrct gets nbits from iword starting at the nstart bit from the
c  right.  An alternative version using only arithmetic operations
c  in subroutine letter.
      ixtrct(nstart,nbits,iword)=mod(iword/(2**(nstart-nbits)),
     $                           2**nbits)+((1-sign(1,iword))/2)*
     $                           (2**nbits-min(1,mod(-iword,
     $                           2**(nstart-nbits))))
c
c  Plot a single special centered symbol
      scale=height/21.0
       ia=nglyph + 1
c  Re-orders special symbols to Prime convention.
      ia=map(ia) + 1
      is=isstar(ia)
       ib=30
c  Unpack the pen position from ssymbc.  ipen=0  means quit.
   20   ipen=ixtrct(ib, 3, ssymbc(is))
       if (ipen.eq.0) return
c  Unpack and scale coordinates.
       ix=ixtrct(ib-3, 6, ssymbc(is))
       iy=ixtrct(ib-9, 6, ssymbc(is))
       xx=scale*(ix-32)
       yy=scale*(iy-32)
       call plot(x+xx, y+yy, ipen)
       ib=45-ib
       if (ib.eq.30) is=is+1
       goto 20
      end
c_______________________________________________________________________
      blockdata hatdat
c  yx  contains character heights and centers of topmost portion
      common /crown/ yx(433)
      data (yx(j),j=1,162)/
     $21.09,21.07,21.11,21.06,21.08,21.08,21.11,21.11,21.04,
     $21.12,21.11,21.04,21.12,21.09,21.10,21.07,21.10,21.07,
     $21.10,21.08,21.11,21.09,21.12,21.10,21.09,21.12,14.11,
     $21.04,14.10,21.15,14.10,21.09,14.11,21.04,22.04,22.06,
     $21.04,21.04,14.14,14.08,14.09,14.08,14.11,14.08,14.08,
     $21.05,14.10,14.08,14.11,14.08,14.08,14.10,21.10,21.11,
     $21.10,21.10,21.13,21.10,21.11,21.12,21.09,21.10,18.13,
     $9.13,25.2,12.13,25.11,25.03,15.08,2.05,2.05,25.10,
     $21.09,21.07,21.08,21.09,21.08,21.12,21.11,21.10,21.04,
     $21.11,21.09,21.12,21.09,21.09,21.10,21.11,21.07,21.07,
     $21.08,21.09,21.10,21.10,21.11,21.10,14.12,21.13,14.09,
     $21.10,14.08,21.10,14.09,21.13,14.06,14.12,21.02,14.12,
     $14.08,21.10,14.09,14.12,14.10,14.12,14.12,14.09,14.16,
     $14.08,21.16,14.13,21.10,21.08,21.12,21.07,21.10,21.10,
     $21.12,21.12,21.06,21.10,21.11,21.06,21.12,21.10,21.11,
     $21.08,21.11,21.08,21.12,21.10,21.11,21.09,21.12,21.09,
     $21.10,21.11,14.09,21.05,14.10,21.15,14.10,21.09,14.10,
     $21.05,21.05,21.06,21.05,21.05,14.13,14.08,14.10,14.08/
      data (yx(j+162),j=1,144)/
     $14.12,14.07,14.10,21.06,14.10,14.08,14.12,14.09,14.09,
     $14.10,21.10,21.11,21.11,21.11,21.13,21.10,21.11,21.10,
     $21.10,21.10,18.13,9.13,25.2,12.13,25.11,25.03,21.08,
     $2.05,2.05,25.10,21.10,21.08,21.09,21.10,21.10,21.11,
     $21.12,21.11,21.06,21.11,21.10,21.12,21.10,22.12,21.11,
     $21.12,21.08,21.08,21.10,21.10,21.10,21.09,21.12,21.11,
     $14.12,21.14,14.12,22.11,14.08,21.11,14.10,21.15,14.06,
     $14.12,21.04,14.13,14.10,21.11,14.10,14.14,14.11,14.12,
     $14.12,14.08,14.16,14.07,21.16,14.13,21.13,21.12,21.15,
     $21.11,21.13,21.13,21.15,21.16,21.10,21.14,21.15,21.10,
     $21.16,21.14,21.13,21.12,21.13,21.12,21.16,21.14,21.14,
     $21.12,21.16,21.13,21.13,21.15,14.12,21.08,14.10,21.18,
     $14.10,21.14,14.12,21.08,21.09,21.10,21.08,21.08,14.15,
     $14.10,14.10,14.09,14.12,14.09,14.10,21.10,14.11,14.10,
     $14.15,14.12,14.11,14.11,21.13,21.14,21.13,21.13,21.17,
     $21.14,21.13,21.12,21.12,21.12,18.13,9.13,25.24,12.13/
      data (yx(j+306),j=1,127)/
     $25.15,25.09,21.10,2.03,2.03,25.14,21.10,21.07,21.11,
     $21.06,21.10,21.10,21.11,21.11,21.04,21.12,21.11,21.04,
     $21.12,21.12,21.10,21.07,21.10,21.07,21.10,21.06,21.11,
     $21.10,21.13,21.10,21.10,21.11,14.13,21.04,14.10,21.15,
     $14.10,21.10,14.13,21.04,21.04,21.04,21.04,21.04,14.12,
     $14.07,14.09,14.07,14.13,14.08,14.08,21.05,14.10,14.08,
     $14.12,14.09,14.08,14.09,21.10,21.11,21.10,21.10,21.14,
     $21.08,21.11,21.10,21.09,21.10,18.12,10.10,25.2,14.10,
     $25.10,25.03,21.08,3.06,3.05,25.09,18.2,18.04,21.11,
     $21.09,14.10,21.2,21.04,14.2,25.2,25.2,13.12,17.12,
     $17.12,16.11,18.13,25.17,10.05,25.22,25.11,21.12,25.09,
     $25.05,21.14,11.09,25.06,25.08,21.14,21.08,21.08,14.05,
     $14.05,21.05,21.09,21.13,21.10,14.06,14.06,21.08,21.14,
     $12.12,25.10,25.04,25.04,21.10,21.07,14.16,21.14,21.10, 4.09/
      end
c_______________________________________________________________________
c  If your computer does not use ASCII representation for characters
c  you will need to translate them for chrcod.  Only 95 characters 
c  have significance; here are their ASCII integers:
c          32    33 !  34 "  35 #  36 $  37 %  38 &  39 '  40 (
c    41 )  42 *  43 +  44 ,  45 -  46 .  47 /  48 0  49 1  50 2
c    51 3  52 4  53 5  54 6  55 7  56 8  57 9  58 :  59 ;  60 <
c    61 =  62 >  63 ?  64 @  65 A  66 B  67 C  68 D  69 E  70 F
c    71 G  72 H  73 I  74 J  75 K  76 L  77 M  78 N  79 O  80 P
c    81 Q  82 R  83 S  84 T  85 U  86 V  87 W  88 X  89 Y  90 Z
c    91 [  92 \  93 ]  94 ^  95 _  96 `  97 a  98 b  99 c 100 d
c   101 e 102 f 103 g 104 h 105 i 106 j 107 k 108 l 109 m 110 n
c   111 o 112 p 113 q 114 r 115 s 116 t 117 u 118 v 119 w 120 x
c   121 y 122 z 123 { 124 | 125 } 126 ~ 
c  You must provide a substitute integer-valued function for  ichar
c  (say  jchar) so that, for example,  jchar('W')=87. The simplest way 
c  to do this is to set up an integer array kode mapping your
c  character set into the above scheme; thus  kode(ichar('W'))=87,
c  where  ichar  is your local instrinsic FORTRAN-77 function.
c  Then  jchar  would be defined as follows:
c        function jchar(kar)
c        character*1 kar
c        dimension kode(128)
c        data kode/ ........../
c        j=ichar(kar)
c        jchar=kode(j)
c        return
c        end
c  Finally, you must substitute your integer code for backslash in place
c  of 92 in the line defining the character variable   bkslsh   in
c  subroutine chrcd
c_______________________________________________________________________
      blockdata blocka
c  Wolcott's blockdata statement reordered for subroutine letter
c  The new ordering is as follows
c   1-26   upper case Roman simplex;    27-52   lower case Roman simplex
c  53-72   simplex numbers and symbols; 73-96   upper case Greek simplex
c  97-120  lower case Greek simplex;   121-146 upper case Roman complex
c  147-172 lower case Roman complex;   173-192 complex numbers & symbols
c  193-216 upper case Greek complex;   217-240 lower case Greek complex
c  241-266 upper case Roman italic;    267-292 lower case Roman italic
c  293-312 italic numbers and symbols; 313-338 upper case Roman duplex
c  339-364 lower case Roman duplex;    365-384 duplex numbers & symbols
c  385-433 special mathematical symbols
c
      integer symbcd,ssymbc
      common /ialph/ width(433),symbcd(4711),istart(433),ssymbc(128),
     $isstar(22)
      common /ofset/ ioff,just1,just2,math
      data ioff,just1,just2,math/0,0,0,-1/
      data (symbcd(j), j=1, 114)/
     $443556555,443557579,432612882,        0,433070987,433071584,
     $323987166,328083226,325854871,317404054,317400725,325723922,
     $327657165,323364299,298156032,462268125,321889760,309339231,
     $300852123,296493907,298329038,304489675,317040204,325527312,
     $        0,433070987,433071456,319792797,325953304,327788240,
     $323429900,312845195,        0,433070987,433071840,432743830,
     $432383691,        0,433070987,433071840,432743830,        0,
     $462268125,321889760,309339231,300852123,296493907,298329038,
     $304489675,317040204,325527312,327792083,327778304,433070987,
     $462432011,432744214,        0,433070987,        0,449848720,
     $312911116,306553867,298197837,294134546,        0,433070987,
     $462431122,443262731,        0,433070987,432383627,        0,
     $433070987,433071499,466625931,466626443,        0,433070987,
     $433071883,462432011,        0,443556959,300852123,296493907,
     $298329038,304489675,317040204,325527312,329885528,328050397,
     $321889760,309329920,433070987,433071584,323987166,328083225,
     $325822102,317367189,        0,443556959,300852123,296493907,
     $298329038,304489675,317040204,325527312,329885528,328050397,
     $321889760,309343631,327450624,433070987,433071584,323987166/
      data (symbcd(j), j =    115,  228)/
     $328083226,325854871,317399958,447424267,        0,460236383,
     $315630752,300917597,296592281,300688471,317367892,323593937,
     $325527116,314942603,300294990,        0,441459851,426780256,
     $        0,433070993,300360780,310748555,321267406,327722784,
     $        0,426779851,460334283,        0,428876875,449848395,
     $449849035,470820555,        0,430974667,460333899,        0,
     $426779862,308655840,309002240,460333899,430974688,430286539,
     $        0,443556555,443557579,432612882,        0,433070987,
     $433071584,323987166,328083226,325854871,317404054,317400725,
     $325723922,327657165,323364299,298156032,433070987,433071776,
     $        0,443556555,443557579,426092235,        0,433070987,
     $433071840,432743830,432383691,        0,460333899,430974688,
     $430286539,        0,433070987,462432011,432744214,        0,
     $443556959,300852123,296493907,298329038,304489675,317040204,
     $325527312,329885528,328050397,321889760,309343382,319488000,
     $433070987,        0,433070987,462431122,443262731,        0,
     $443556555,443557579,        0,433070987,433071499,466625931,
     $466626443,        0,433070987,433071883,462432011,        0,
     $428877472,436938134,428189323,        0,443556959,300852123/
      data (symbcd(j), j =    229,  342)/
     $296493907,298329038,304489675,317040204,325527312,329885528,
     $328050397,321889760,309329920,433070987,462432011,433071904,
     $        0,433070987,433071584,323987166,328083225,325822102,
     $317367189,        0,428877014,293974816,324023051,323321856,
     $441459851,426780256,        0,428712733,296723360,303047775,
     $307143897,308655771,323921503,319825312,313500957,309100544,
     $445654283,441295834,298623831,296362898,300459152,315106897,
     $323561172,325822105,321725851,307068928,430974667,430286560,
     $        0,447751499,428680026,298623957,302621778,310945169,
     $321463955,325756697,330114970,        0,430285899,298394454,
     $296559517,303015136,313533983,323921626,325789330,317040331,
     $        0,455910987,455812568,313304217,302785430,296330065,
     $298263564,306554187,317072974,        0,433070987,432743448,
     $307012953,317466198,323593873,321332684,312845451,302392206,
     $        0,455812568,313304217,302785430,296330065,298263564,
     $306554187,317072974,        0,456140363,455812568,313304217,
     $302785430,296330065,298263564,306554187,317072974,        0,
     $430548563,321562135,317465945,307012632,298525523,296264590,
     $302392459,312845772,321323008,445654176,303014876,300266265/
      data (symbcd(j), j =    343,  456)/
     $309100544,455910985,318973381,312616068,302167638,317465945,
     $307012632,298525523,296264590,302392459,312845772,321323008,
     $433070987,432710744,309110169,319563349,321224704,430973855,
     $300950433,296760217,298156032,435168287,305144865,300954649,
     $302261189,295838404,        0,433070987,453813135,441034315,
     $        0,433070987,        0,432841611,432710744,309110169,
     $319563349,321238613,327952281,338471128,344631563,        0,
     $432841611,432710744,309110169,319563349,321224704,441230360,
     $298525523,296264590,302392459,312845772,321332881,323593814,
     $317465945,307003392,432841604,432743448,307012953,317466198,
     $323593873,321332684,312845451,302392206,        0,455910980,
     $455812568,313304217,302785430,296330065,298263564,306554187,
     $317072974,        0,432841611,432645078,304882905,315392000,
     $453715416,311207001,298591062,298460179,313075153,319268366,
     $317072651,304456588,296157184,435168207,302392459,310752025,
     $309100544,432841615,300295243,310748556,321369689,321224704,
     $428647563,453813387,        0,430744651,447521867,447522379,
     $464299595,        0,430745099,453813067,        0,428647563,
     $453813387,302228357,293741252,        0,453813067,430745113/
      data (symbcd(j), j =    457,  570)/
     $430286347,        0,443327576,300622740,296264526,298198027,
     $306554124,317171282,325789465,443327833,315368918,321332876,
     $325429003,        0,449848607,307143705,300622738,296100612,
     $449848864,323954331,321693208,315335895,443262294,317335058,
     $319268301,314975499,306553868,300327824,        0,426451800,
     $300721177,306980055,311043344,308655833,323692116,308651079,
     $302120960,447521945,302785430,296330064,298230732,304456907,
     $312878542,319333908,317433177,309175453,307209440,313533919,
     $321814528,451650968,311207001,300688342,302654675,443130834,
     $296231758,298198027,308651340,317128704,445654175,305079389,
     $307111259,319665691,311206999,298459985,296199053,302359753,
     $310617349,308421700,302186496,426418967,298624025,304882774,
     $302588811,436806806,311174553,319596183,323626575,314703872,
     $426418967,298624025,304882774,302556174,304489611,310748556,
     $319268433,323626713,325985951,319825312,313468252,315401750,
     $323626834,        0,437035922,296166220,298165259,306619599,
     $        0,437035787,457975385,319595928,306848787,300528595,
     $304686225,310781259,314942924,        0,426779488,300917790,
     $319141017,293961728,439132868,436904912,300328011,308651340/
      data (symbcd(j), j =    571,  684)/
     $317138514,460105298,319235596,321234635,329688975,        0,
     $430744601,300524430,296072857,321594900,315139278,302392139,
     $        0,445654175,305079389,307111259,319665499,307045401,
     $300655573,304719122,315176210,302556048,296166220,300229832,
     $310617349,306324484,        0,441230360,298525523,296231821,
     $300295243,308651340,317138449,319432151,315368729,307003392,
     $443327435,453813843,323430091,428549016,304916377,        0,
     $432645008,300327948,306554123,314975758,321431124,319530456,
     $313304281,304882646,298427012,        0,462202009,302785430,
     $296330064,298230732,304456907,312878542,319333908,317433240,
     $311197696,447521931,428549016,304916249,        0,426418967,
     $298624025,304882774,300426189,304456907,314975758,323561174,
     $325877760,441197591,298492754,296199053,300295243,310748620,
     $323430161,329918295,325887577,317433171,308749316,        0,
     $428647321,302753158,318908036,460105367,319431561,293806788,
     $        0,458237060,426418967,298624025,304882774,302556174,
     $304489675,312845836,323430161,332081113,        0,441230360,
     $298492754,296199052,300262475,308684111,449422671,314975691,
     $321234636,329754514,332048216,327974912,445653835,445654731/
      data (symbcd(j), j =    685,  798)/
     $445556363,434677265,426091595,451258187,        0,435168203,
     $437265419,428877344,326084382,330180442,327952087,319501856,
     $323987166,328083226,325854871,319501334,319497941,327821138,
     $329754381,325461515,293975574,323659476,327755535,325494412,
     $319127552,460236570,328214237,321889696,311436383,300852123,
     $296493907,298329038,304489739,314943052,325527312,445654175,
     $302949339,298591123,300426254,306586891,        0,435168203,
     $437265419,428877216,321890013,328050520,329885456,325527116,
     $314942219,449848863,323921627,327952147,325592718,319169931,
     $        0,435168203,437265419,449652114,428877600,328017632,
     $436938134,428189451,327722699,        0,435168203,437265419,
     $449652114,428877600,328017632,436938134,428188875,        0,
     $460236570,328214237,321889696,311436383,300852123,296493907,
     $298329038,304489739,314943052,325530912,307209245,300786584,
     $298427344,302457996,310752979,325433107,327530003,334069760,
     $435168203,437265419,462432011,464529227,428877024,456140832,
     $436938518,428188875,455452683,        0,435168203,437265419,
     $428877024,428188875,        0,445654287,308683851,300262220,
     $294069008,296264592,296203488,308782220,304460832,317718528/
      data (symbcd(j), j =    799,  912)/
     $435168203,437265419,464528403,447457099,445359883,428877024,
     $456140768,428188875,455452619,        0,435168203,437265419,
     $428877024,428189387,325625483,        0,435168203,437265806,
     $435168651,464528779,464529227,466626443,428876832,464529504,
     $428188811,457549899,        0,435168203,437266189,437200651,
     $462432011,428876832,456140768,428188811,        0,445654111,
     $300852123,296461140,298329038,304489739,314943052,325527312,
     $329918295,328050397,321889696,311440672,307209245,300786583,
     $298460112,302457996,310752651,319170190,325592852,327919323,
     $323921439,315621376,435168203,437265419,428877344,326084382,
     $330180441,327919318,319464469,454043295,326051612,327984855,
     $323692053,428188875,        0,445654111,300852123,296461140,
     $298329038,304489739,314943052,325527312,329918295,328050397,
     $321889696,311440672,307209245,300786583,298460112,302457996,
     $310752651,319170190,325592852,327919323,323921439,315634765,
     $304555152,310945105,317203982,321103494,327362376,329561614,
     $321201800,325297927,329515008,435168203,437265419,428877344,
     $326084382,330180442,327952087,319497238,454043295,326051612,
     $328017624,323724822,428188875,447423957,319432397,327558988/
      data (symbcd(j), j =    913,  1026)/
     $331789781,319399564,325429067,331786126,        0,458139360,
     $325920413,319792480,307241951,296657755,298623960,304850389,
     $321529554,430810073,304883158,321562260,325658318,321267083,
     $308651020,298263377,296067982,        0,443557067,445654283,
     $430973722,294659808,325920416,436577739,        0,435168209,
     $302457996,312845771,323364622,329820000,437265425,304555212,
     $312849184,309343904,336592896,430974219,433071374,460334347,
     $426779744,451946336,        0,433071243,435168400,449848459,
     $449848971,451946128,466626187,426779808,460335200,        0,
     $430974603,433071819,460333899,426779744,451946336,426091595,
     $451258187,        0,430974229,310752160,313173323,462431573,
     $426779744,454043552,438674955,        0,458236747,460333963,
     $433070938,296756960,430286539,325625483,        0,445653835,
     $445654731,445556363,434677265,426091595,451258187,        0,
     $435168203,437265419,428877344,326084382,330180442,327952087,
     $319501856,323987166,328083226,325854871,319501334,319497941,
     $327821138,329754381,325461515,293975574,323659476,327755535,
     $325494412,319127552,435168203,437265419,428877536,325920416,
     $428188875,        0,445653771,445654795,445556427,430319308/
      data (symbcd(j), j =    1027,  1140)/
     $428189451,        0,435168203,437265419,449652114,428877600,
     $328017632,436938134,428189451,327722699,        0,458236747,
     $460333963,433070938,296756960,430286539,325625483,        0,
     $435168203,437265419,462432011,464529227,428877024,456140832,
     $436938518,428188875,455452683,        0,445654111,300852123,
     $296461140,298329038,304489739,314943052,325527312,329918295,
     $328050397,321889696,311440672,307209245,300786583,298460112,
     $302457996,310752651,319170190,325592852,327919323,323921439,
     $315634841,306787865,319370390,319501461,319455232,435168203,
     $437265419,428877024,428188875,        0,435168203,437265419,
     $464528403,447457099,445359883,428877024,456140768,428188875,
     $455452619,        0,445653835,445654731,445556363,426091595,
     $451258187,        0,435168203,437265806,435168651,464528779,
     $464529227,466626443,428876832,464529504,428188811,457549899,
     $        0,435168203,437266189,437200651,462432011,428876832,
     $456140768,428188811,        0,433103708,464561948,441197651,
     $455878163,432513866,463972106,433039135,433006366,441132566,
     $441099797,432449293,432416524,        0,445654111,300852123,
     $296461140,298329038,304489739,314943052,325527312,329918295/
      data (symbcd(j), j =    1141,  1254)/
     $328050397,321889696,311440672,307209245,300786583,298460112,
     $302457996,310752651,319170190,325592852,327919323,323921439,
     $315621376,435168203,437265419,462432011,464529227,428877856,
     $428188875,455452683,        0,435168203,437265419,428877344,
     $326084382,330180441,327919318,319464469,454043295,326051612,
     $327984855,323692053,428188875,        0,430974230,293974816,
     $309015328,326117146,324023116,323367691,325429009,323321856,
     $443557067,445654283,430973722,294659808,325920416,436577739,
     $        0,428712733,296723360,303047775,307143897,308654877,
     $298820639,307148507,326018719,321922528,315598173,311207179,
     $460236383,317695325,436577739,        0,445654283,447751499,
     $441295834,298623831,296362898,300459152,317204113,325658388,
     $327919321,323823067,307082395,302851033,298558356,300491793,
     $306722256,321431186,325723863,323790426,317568096,319829067,
     $319127552,430974603,433071819,460333899,426779744,451946336,
     $426091595,451258187,        0,447751499,449848715,428647258,
     $300721173,304718994,310948698,298623957,302621778,310945233,
     $323561171,327853913,332215761,321463955,325756697,332212185,
     $441460320,440772171,        0,430384011,306553871,298427222/
      data (symbcd(j), j =    1255,  1368)/
     $296559517,303015136,317728415,328116058,329983763,323462667,
     $327526222,436708306,298525594,300852319,309343712,321890013,
     $328017686,325658255,432415820,455485196,        0,434873302,
     $298525591,300688473,313304536,319530581,321332876,325432855,
     $319235660,325429003,453682644,304718738,296231758,298198091,
     $310748556,319239251,300491664,298263500,304447488,435168203,
     $437265419,436937880,311207321,321660630,327788305,325527116,
     $314942731,306586638,449619480,323692243,325625486,319169931,
     $428876832,        0,455812629,321529493,323692056,315401433,
     $302785430,296330065,298263564,308651339,319170190,443327576,
     $300622739,298361806,304489675,        0,456140363,458237579,
     $455812568,313304281,302785430,296330065,298263564,308651339,
     $317072974,443327576,300622739,298361806,304489675,449848992,
     $455452491,        0,432645779,323659351,319563161,309109784,
     $298525523,296264590,302392523,312845836,323434067,321594904,
     $443327576,300622739,298361806,304489675,        0,445621470,
     $311338334,313500960,307242015,300852171,441459807,302949387,
     $428647705,428188875,        0,441230360,300655509,298427345,
     $302523535,310879632,317236755,319464919,315368729,307016728/
      data (symbcd(j), j =    1369,  1482)/
     $300622802,302527888,317269462,315373015,319563417,323757592,
     $434676624,296166221,298165322,314910281,323236685,298198091,
     $314943050,323233415,321037700,302129989,293839624,296035339,
     $        0,435168203,437265419,436937880,313304537,323757782,
     $325432793,321660566,323334944,303051531,308655563,331710464,
     $435168159,300885023,300954585,300266521,302363417,302822155,
     $308641792,437265375,302982239,303051865,304325637,297935620,
     $291676870,293839686,293778457,302228421,297939801,304906240,
     $435168203,437265419,458007567,447325899,445228683,428876832,
     $451716953,428188875,451258187,        0,435168203,437265419,
     $428876832,428188875,        0,434938827,437036043,436937880,
     $313304537,323757782,325432793,321660566,323335894,330049561,
     $340568408,348858763,474786072,346761547,428647449,428188875,
     $451258251,474327627,        0,434938827,437036043,436937880,
     $313304537,323757782,325432793,321660566,323334937,302822155,
     $308655563,331710464,443327512,298525523,296264590,302392523,
     $312845836,323430097,325691030,319563097,309114073,304882646,
     $298427281,300360780,308655435,317072974,323528339,321594840,
     $313294848,434938820,437036036,436937880,311207321,321660630/
      data (symbcd(j), j =    1483,  1596)/
     $327788305,325527116,314942731,306586638,449619480,323692243,
     $325625486,319169931,428647449,427959492,        0,455910980,
     $458008196,455812568,313304281,302785430,296330065,298263564,
     $308651339,317072974,443327576,300622739,298361806,304489675,
     $448931652,        0,434938827,437036043,436839510,309077337,
     $319596120,321627670,317433368,428647449,428188875,        0,
     $451651097,319464919,315368729,302818200,296461141,298460179,
     $313042384,319271766,298492948,313075153,319301133,317072715,
     $304456652,298230607,296067981,        0,435168207,302392459,
     $310748556,317142048,302490700,306557721,311197696,434938830,
     $302392523,312845836,323433497,302457932,308655769,323335897,
     $325432089,302822873,325891723,331710464,430744779,432841933,
     $455910603,426550361,447522521,        0,432841867,434939022,
     $449619083,449619595,451716750,466396811,426550425,460105817,
     $        0,432842315,434939531,458007435,428647577,449619737,
     $428188811,449160971,        0,432841995,434939149,458007819,
     $306422789,297935684,293774150,297972505,307017113,327974912,
     $453813067,455910283,432841557,296527449,430286411,321365515,
     $        0,445424728,300622740,296264526,298198091,308651340/
      data (symbcd(j), j =    1597,  1710)/
     $319268498,327886681,445424792,302719956,298361742,300295243,
     $445425049,319563350,325527308,329627033,317466134,323430092,
     $329623435,        0,451945759,307143705,300622738,296100612,
     $451945823,309240921,302719954,298197828,451946080,326084382,
     $328050393,323757527,309048928,326051547,323790424,317437143,
     $317400660,323561103,321299980,312845515,304489485,300430551,
     $315303444,321463887,319202764,312836096,426451800,300721241,
     $309077271,313140560,310780996,428581784,306980119,462202582,
     $323626317,306455556,460105366,321529165,        0,451683673,
     $309109784,298492754,296199053,300295243,308651404,319268434,
     $321562135,311305438,309339425,315663904,323957977,304882645,
     $298394510,300299467,312878543,319366678,317465947,311338271,
     $313533920,323944448,455812568,313304153,300688342,304751891,
     $439133208,302720148,311014675,300491600,296166284,304456971,
     $314975758,445228050,298328974,300295243,        0,447751391,
     $307176605,309208475,325953244,319661337,304849812,296264527,
     $298230859,310682951,312648964,306324549,449651863,300557201,
     $298296269,304447488,426418967,298624089,306979990,304686027,
     $437036120,304817170,298169426,309011800,317498969,325854999/
      data (symbcd(j), j =    1711,  1824)/
     $327821007,318912089,325822164,323462596,        0,426418967,
     $298624089,306979990,304653390,306586827,437036120,304817169,
     $302457932,308651339,317072974,325625620,330082141,328181408,
     $319825310,315499993,321595092,331953612,321365649,325723929,
     $328115935,324009984,437035922,296166220,298165323,308716815,
     $439133138,298263436,300253184,437035787,439133003,458008280,
     $327952089,321693144,308946003,300528723,308880716,314946643,
     $306783500,312845771,321267407,        0,430973920,305112222,
     $309208654,323364555,435168350,307111438,321267403,327529753,
     $293975321,296058880,439132868,441230084,439034896,302425227,
     $310748556,319235729,462202446,321267339,329623501,336050009,
     $323430028,325419008,437035915,439133203,300360587,460105365,
     $319338265,325789332,319333775,308716620,298169177,304906240,
     $447751391,307176605,309208475,321762715,307045401,300655573,
     $304719122,317273499,309142617,302752789,306816274,445195281,
     $298328910,296100810,310650183,312648900,304231698,304653264,
     $298263436,302327048,        0,443327512,298492754,296199053,
     $300295243,308651404,319268434,321562135,317465945,309114073,
     $304882645,298394510,300299467,312878543,319366678,317456384/
      data (symbcd(j), j =    1825,  1938)/
     $443294667,443294731,455878219,455878283,428549016,304916377,
     $428549015,304883608,        0,432546765,302392459,310748620,
     $321365650,323659351,319563161,311207000,300589970,289551627,
     $314975759,321463894,319567129,306979861,300491460,        0,
     $464299225,302785429,296297295,298230732,304456907,314975759,
     $321463893,319530456,313308377,304882645,298394510,300299467,
     $312878543,319366678,317470168,330039296,447489163,447489227,
     $428549016,304916249,428549015,304883480,        0,426418967,
     $298624089,306979990,302523405,306557977,304882774,300426189,
     $302392459,308651404,319235729,325723863,323790424,323725012,
     $457746135,        0,441197591,298492754,296199053,300295243,
     $310748620,323430161,329918295,325887577,317433171,308749316,
     $430416845,304489740,317105807,327726935,325854808,317400403,
     $308716612,        0,428647321,302785622,314811845,318911385,
     $300688406,312714629,318908036,460105367,319431561,293806788,
     $        0,456139972,458237060,426418967,298624089,306979990,
     $304653390,308684172,319203024,329888793,304882774,302556174,
     $304489675,314942988,323430161,329885657,        0,432710679,
     $309077145,302785429,296297295,298197963,304456908,312976786/
      data (symbcd(j), j =    1939,  2052)/
     $430416781,300295244,308716879,447292751,314975691,321234636,
     $329754514,332048216,327984856,330016661,447194509,317072972,
     $325494607,        0,451945099,451945995,449783243,432580049,
     $419799947,444966539,        0,443556683,445653899,437266144,
     $332376029,334342040,330016406,460334943,332310427,330049303,
     $323695702,323692309,329885521,327624332,314942091,457909973,
     $327788305,325527116,314933248,462366558,332408666,330180382,
     $326084192,315630815,305046490,298558291,296231821,300295307,
     $312845772,321332880,449848607,307143706,300655507,298329037,
     $302392459,        0,443556683,445653899,437266016,328181598,
     $332244887,329885391,321299916,308650635,456140511,328148827,
     $330016531,323462669,314975435,        0,443556683,445653899,
     $453846418,437266400,332212128,439035350,423994955,325592587,
     $        0,443556683,445653899,453846418,437266400,332212128,
     $439035350,423994443,        0,462366558,332408666,330180382,
     $326084192,315630815,305046490,298558291,296231821,300295307,
     $310748620,321332946,449848607,307143706,300655507,298329037,
     $302392459,444966284,319235730,451487634,        0,443556683,
     $445653899,470820491,472917707,437265888,464529696,439035734/
      data (symbcd(j), j =    2053,  2166)/
     $423994443,451258251,        0,443556683,445653899,437265888,
     $423994443,        0,456140047,308716684,302359435,294003406,
     $292037393,296231695,454042831,306619403,447751968,        0,
     $443556683,445653899,472917011,451651275,449554059,437265888,
     $464529632,423994443,451258187,        0,443556683,445653899,
     $437265888,423994955,325625355,        0,443556683,443557131,
     $445654349,472917259,472917707,475014923,437265696,472918368,
     $423994379,453355467,        0,443556683,443557518,443459211,
     $470820491,437265632,464529632,423994379,        0,449848543,
     $305046490,298558291,296231821,300295243,310748620,321332945,
     $327821144,330147614,326084192,315635104,311403677,302851031,
     $298427280,300328011,444966284,319235729,325723928,328050398,
     $321912832,443556683,445653899,437266208,334473245,336439256,
     $329983573,304789280,332376029,334342040,327886421,423994443,
     $        0,449848543,305046490,298558291,296231821,300295243,
     $310748620,321332945,327821144,330147614,326084192,315635104,
     $311403677,302851031,298427280,300328011,444966284,319235729,
     $325723928,328050398,321926093,300360720,306750673,313009550,
     $314811846,321070728,323270030,316941831,321103496,        0/
      data (symbcd(j), j =    2167,  2280)/
     $443556683,445653899,437266144,332376029,334342040,330016406,
     $304821984,330278813,332244824,327919254,449521173,321529484,
     $325429067,331786126,455747277,327558988,331788939,304447488,
     $464463774,334505882,332277598,328181344,313533599,302949403,
     $304915608,321529554,437101721,321562260,325658319,323397196,
     $314942603,300295053,296198993,293970765,298221568,451945547,
     $454042763,439362458,303048672,332212128,432383307,        0,
     $441459669,298361742,300295307,314943052,325527313,336606432,
     $302687185,300360716,306557920,315635552,342884352,437265483,
     $439362701,466625611,433071392,458237984,        0,441459723,
     $443556941,458236939,458237451,460334669,475014667,435168672,
     $468724064,        0,439363083,441460299,468722379,435168608,
     $460335200,421897163,447063755,        0,437265686,304460896,
     $313205899,468723030,433071392,460335200,432383307,        0,
     $466625227,468722443,441459674,305145824,426092107,325625355,
     $        0,466527124,331710464,432973716,298156032,455747095,
     $317465945,309109784,298492754,296199053,300295243,308651404,
     $319235665,323692187,321857055,315630816,305112094,302949469,
     $305083609,304882645,298394510,300299467,312878542,319333974/
      data (symbcd(j), j =    2281,  2394)/
     $321758750,315621376,428877067,430974221,462431499,428877600,
     $430941919,        0,453780889,309109784,298525523,296231821,
     $300295307,312845772,443327576,300622739,298329037,302392459,
     $432612754,        0,466625433,331953040,331887499,331710464,
     $433072025,298398608,331887499,331710464,468166479,325592658,
     $315303255,309077080,300655509,298427345,304620752,313042322,
     $321595096,330082265,        0,468821922,334538786,336701412,
     $330442467,321955359,317597080,310781128,306394786,321922588,
     $315106636,310682823,304260036,295838469,293806919,298001221,
     $        0,468821922,334538786,336701412,330442467,321955359,
     $317597080,310781128,306394786,321922588,315106636,310682823,
     $304260036,295838469,293806919,298001221,447587482,302785493,
     $300524560,306652493,317105806,327690067,329951000,323823067,
     $313360384,470394833,329787088,321431058,313206039,306979864,
     $298558293,296330129,302523536,310945106,319497815,325855064,
     $334211093,336166912,449717643,432678804,432383883,        0,
     $449717643,432940956,432678804,        0,432908045,462267277,
     $        0,451847580,317564444,317633428,336213453,314975691,
     $319169997,        0,439493700,441590916,479340804,481438020/
      data (symbcd(j), j =    2395,  2508)/
     $431106660,430056836,469903940,        0,434807700,300524564,
     $300580864,430744665,317109273,317044772,317030400,435299926,
     $297939876,319501156,319468388,345123229,343028677,344109956,
     $344074635,341966848,447751327,302916570,298558290,296166284,
     $302359691,312878543,319333972,323790493,321889760,313537888,
     $309306460,302851031,298394510,300295179,440771852,315074001,
     $319432281,321824287,317731798,319488000,443688035,303113184,
     $300885020,304981145,306947093,439460897,303015005,307111130,
     $309077142,298460306,308815054,306586699,302294023,304264211,
     $306750607,304522252,300229576,302195781,308412416,435299427,
     $307307744,309273756,304981017,302752917,439461025,307209309,
     $302916570,300688406,311043090,300426190,302392395,306488455,
     $304264339,302556175,304522380,308618440,306390085,300023808,
     $462169818,321758619,311239897,306914451,308847952,319301265,
     $325694875,311207126,308913425,313014043,325691089,329787344,
     $338241685,340502618,336471966,328181344,315630815,305079260,
     $298656599,296362897,300393549,308684171,321234700,331786190,
     $464365331,327722832,        0,426321109,325661394,309012178,
     $        0,298394766,308651209,306390020,300032901,295936842/
      data (symbcd(j), j =    2509,  2622)/
     $298263570,306881880,317498969,327952214,329852686,323364363,
     $317040012,315041231,319235533,455911128,327886610,325527180,
     $        0,458008082,317138380,319137483,329688975,460105298,
     $319235596,321238546,319464920,313304281,302785429,296297295,
     $298230732,304456907,312878543,319370457,304882645,298394510,
     $300285952,441459603,298329037,302396640,300528595,302720152,
     $311207321,319563351,323659410,321365452,310748299,302392271,
     $300529176,321594962,319268236,310752224,309329920,453715477,
     $321562198,319563161,309109784,298492754,296199053,300295243,
     $308651404,319272153,304882645,298394510,300285952,462431762,
     $317138380,319137483,329688975,464528978,319235596,321238546,
     $319464920,313304281,302785429,296297295,298230732,304456907,
     $312878543,319370457,304882645,298394510,300299872,330301440,
     $432546961,313075220,321594904,315401433,302785429,296297295,
     $298230732,304456907,314975758,443327576,300589970,298263500,
     $        0,456107550,321824414,323987040,317728095,311370972,
     $307012555,298033989,451945822,311305432,304587787,300163974,
     $295871172,287449605,285418055,289612357,432842265,        0,
     $460105163,314844421,304227204,293774022,291742472,295936774/
      data (symbcd(j), j =    2623,  2736)/
     $458007947,312747205,304231954,319464920,313304281,302785429,
     $296297295,298230732,304456907,312878543,319370457,304882645,
     $298394510,300285952,441459467,443556683,434709590,309077337,
     $317498968,323724949,319268364,321238489,321627733,317171148,
     $319137483,329688975,435168480,        0,443557023,309273887,
     $309342933,294364057,304915608,306881551,302392395,437036120,
     $304784335,300295179,308651341,315064320,445654239,311371103,
     $311440149,296461273,307012824,308978699,300163974,295871172,
     $287449605,285418055,289612357,439133336,306881483,298066758,
     $291635200,441459467,443556683,457975383,323692247,325854873,
     $321693144,308946003,300528723,308880716,314946643,306783500,
     $312845771,321267407,435168480,        0,441459602,296166220,
     $298165323,308716815,443556818,298263436,300266464,309329920,
     $426418967,298624089,306979990,304686027,437036120,304817170,
     $298169426,309011800,317498969,325854999,327853643,455911127,
     $325756427,459876182,334243929,342665560,348891541,344434956,
     $346405081,346794325,342337740,344304075,354855567,        0,
     $426418967,298624089,306979990,304686027,437036120,304817170,
     $298169426,309011800,317498969,325854999,327853711,323364555/
      data (symbcd(j), j =    2737,  2850)/
     $455911127,325756495,321267339,329623501,336035840,443327512,
     $298492754,296199053,300295243,308651404,319268434,321562135,
     $317465945,309114073,304882645,298394510,300299467,312878543,
     $319366678,317456384,426418967,298624089,306979990,304685892,
     $437036120,304817170,293745746,306881816,315401753,323757783,
     $327853842,325559884,314942731,306586703,304690840,325789394,
     $323462668,314946116,302120960,458007812,460105028,453584405,
     $317465945,309109784,298492754,296199053,300295243,308651340,
     $317171218,443327576,300589970,298263500,438445572,        0,
     $426418967,298624089,306979990,304686027,437036120,304817170,
     $298169426,309011800,317498969,323757719,321594903,321650688,
     $453748246,321594967,319563097,307012568,298558357,300557712,
     $317174678,300590481,317203917,314975435,302359372,294036238,
     $296166221,        0,443556818,298263436,300262539,310814031,
     $445654034,300360652,302363481,315392000,426418967,298624089,
     $306979989,302490637,306557977,304882773,300393421,302392459,
     $310748556,319235730,462202514,321332812,323331915,333883407,
     $464299730,323430028,325419008,426418967,298624089,306979989,
     $302490637,306557977,304882773,300393421,302392459,308651404/
      data (symbcd(j), j =    2851,  2964)/
     $319235729,325756633,323790551,        0,426418967,298624089,
     $306979989,302490637,306557977,304882773,300393421,302392459,
     $310748556,319235664,460105296,321300108,327526283,335947918,
     $342370580,344762585,344700697,323495565,327516160,430613464,
     $304915737,313238868,443327767,311043280,306652172,298165067,
     $294003469,296166285,296105168,308716811,317040204,325564120,
     $323725014,327919384,325887641,319563158,313140496,310814027,
     $        0,426418967,298624089,306979989,302490637,306557977,
     $304882773,300393421,302392459,310748556,319235730,464299595,
     $319038853,308421636,297968454,295936904,300131206,462202379,
     $316941637,308412416,460105367,319464463,298230603,432710615,
     $304915737,319534039,304882968,319530647,432448525,310781388,
     $321303565,310748619,321300111,        0,433202052,435299268,
     $433202532,432153924,        0,443688132,445785348,431105316,
     $430056708,        0,447751044,460334340,432711445,430417615,
     $        0,447653148,313370012,315532639,309339232,300917661,
     $298689497,304850324,434939158,315237842,317203854,310785048,
     $298525524,296297360,302458187,432547021,312845705,314811717,
     $308421700,300065671,298066889,302261191,        0,441459806/
      data (symbcd(j), j =    2965,  3078)/
     $307111134,307246240,306328725,304686212,308880533,428647320,
     $302818202,294433561,319599897,315368985,315434265,        0,
     $434938776,300655640,300725197,298197963,302392269,        0,
     $434938776,300655640,300725195,298197965,302392330,300163975,
     $        0,435168158,300491806,300954590,300692429,298197963,
     $302392269,        0,432939995,298656603,296625054,300917856,
     $311436767,319759964,321725976,317433045,308884768,315598302,
     $319694362,317465942,442934412,308651276,308707328,468722507,
     $441459998,311305434,304915417,296592221,298820640,307242271,
     $317662878,330278880,459875921,319268365,323331851,331753422,
     $333981522,325648384,468461463,334178327,336340953,332179288,
     $327886481,319235468,310748235,298197838,296264595,311141785,
     $317564381,315598112,307209309,304981144,311076430,325461899,
     $333817868,335983691,300295054,298361811,304788571,307013262,
     $327559051,        0,437035992,302752856,302822221,294003531,
     $298188800,437035992,302752856,302822219,294003533,298197899,
     $296002247,        0,441459807,300528799,300528800,309306323,
     $430351116,296067980,296124416,439231643,304948251,302916702,
     $307209568,321922847,330213211,327984856,313205973,308913426/
      data (symbcd(j), j =    3079,  3192)/
     $315176544,326084381,328050393,323757591,440837196,306554060,
     $306610176,430482259,298525719,306947350,319399570,327755667,
     $334148435,298492950,306914581,319366801,327722898,334145495,
     $        0,445784916,310509568,433202516,297926656,433202052,
     $        0,435168153,437265305,451945881,454043033,        0,
     $323397323,441131922,296231758,298197835,430449612,432612240,
     $300360652,296072531,323761693,319628888,325854938,321758749,
     $453944922,325844992,437265311,296657755,298624024,306980121,
     $313369949,311403680,303038464,464201748,329856665,334112399,
     $432678868,        0,454042756,456139844,445424664,298525523,
     $296231822,302392523,314943116,327624529,329918230,323757529,
     $311211289,304882646,298427280,300360780,308655499,321267406,
     $327722772,325789272,317489152,443557017,445654169,        0,
     $306787478,304751824,306652240,308946070,441001092,440673350,
     $306324678,306459417,298591257,298656537,428647961,445425048,
     $319595930,311210763,298132491,298197771,428189195,444966282,
     $319137164,310738944,443556895,298722135,296362895,302392523,
     $312845836,323462868,325822108,319792480,309329920,437134493,
     $313533771,        0,432907164,300885023,307242400,319792734/
      data (symbcd(j), j =    3193,  3306)/
     $323888794,321660373,296068811,        0,435168928,311174616,
     $321627798,325691089,323429900,312845451,300295053,296189952,
     $451945298,327759328,317030400,456139744,298558424,307012953,
     $319563414,325691089,323429900,312845451,300295053,296189952,
     $458139231,315630880,305112028,298558354,300360780,310748491,
     $319170190,325625554,323659287,313271576,304849877,298385408,
     $460334155,430974688,        0,441459679,298754971,300721240,
     $313239062,323626706,325559949,321267083,306553804,298230607,
     $296297364,302720215,317466201,323856029,321889696,307232768,
     $458008150,317334803,308913172,298525529,296559517,303015136,
     $311436767,321824409,323626575,317072651,306553804,298254336,
     $451847627,432678932,        0,432678932,        0,466756356,
     $        0,432777239,432580625,        0,447882466,305112027,
     $298525586,300328009,308487492,        0,431104994,305112283,
     $311108882,308716617,300098372,        0,441263246,430679505,
     $451650385,        0,436609995,298197965,302392330,300163975,
     $        0,434545548,300262412,300318720,441590919,449979783,
     $460236383,315630752,300917597,296592281,300688471,317367892,
     $323593937,325527116,314942603,300294990,        0,443556895/
      data (symbcd(j), j =    3307,  3420)/
     $298722135,296362895,302392523,312845836,323462868,325822108,
     $319792480,309343456,305112094,300819351,298460111,302425164,
     $308655435,317072909,321365652,323724892,319759839,313524224,
     $437134493,313533771,445621515,436577867,        0,432939995,
     $298656603,296625054,300917920,315631199,323954396,325920408,
     $317400212,302621585,296166219,449848863,321857180,323823192,
     $315303060,430351246,302458188,319170189,325530638,312845899,
     $323364558,325582848,432939995,298656603,296625054,300917920,
     $315631199,323921562,321660311,309048736,319792733,321725976,
     $315340183,319497876,325658319,323397196,314942603,300295053,
     $296198992,298361808,298301013,323561103,321299980,314933248,
     $449783179,451945931,451945233,327726283,323321856,435168086,
     $430646232,307012953,319563414,325691089,323429900,312845451,
     $300295053,296198992,298361808,298300761,317466198,323593873,
     $321332684,312849376,321926111,311404128,        0,456042012,
     $321758876,323921503,317728032,305112029,298689367,296264590,
     $302392523,312845836,323430097,325658261,319530328,311174231,
     $300589970,445654175,302949339,298558353,300360780,308655435,
     $317072974,323528338,321562071,313262080,430973786,430842782/
      data (symbcd(j), j =    3421,  3534)/
     $303047840,317630045,323954400,433005599,307209693,460334813,
     $323822997,313107728,310752922,313173267,308815051,        0,
     $441459679,298754970,300688535,315336280,323823261,321889696,
     $307246240,303014877,300753944,306951575,319563354,321824287,
     $315634839,300622741,296330063,298230732,306554251,321267341,
     $325560019,323659350,315339927,302719957,298427279,300327948,
     $306558347,319170125,323462803,321562134,315326464,458008150,
     $317334803,308913172,298525529,296559517,303015136,313533983,
     $323921626,325723792,321332684,310748235,300295054,298296272,
     $302490574,443130964,300622745,298656733,305112288,447751647,
     $321824410,323626576,319235468,310738944,451847627,432678932,
     $        0,432678932,        0,466756356,        0,432777239,
     $432580625,        0,447882466,305112027,298525586,300328009,
     $308487492,443622494,302883798,300491789,304424134,        0,
     $431104994,305112283,311108882,308716617,300098372,435233886,
     $307078358,308880525,304423878,        0,441459860,430876119,
     $451846999,        0,434480012,300327948,302326728,298024960,
     $434545548,300262412,300318720,441590919,449979783,458139228,
     $323856092,326018655,315630752,300917597,296592281,300688471/
      data (symbcd(j), j =    3535,  3648)/
     $317367892,325661531,300721240,317400661,323626706,325527116,
     $314942603,300294990,296199056,300393358,        0,449848543,
     $305046490,298558291,296231821,300295243,308651404,319235729,
     $325723928,328050398,323986976,315635104,311403677,302851031,
     $298427280,300328011,442869068,317138513,323626712,325953182,
     $319815680,449717323,454042763,454042973,307078170,451847387,
     $302841856,439231643,304948251,302916702,307209568,319825631,
     $328115995,325887575,315270291,300458831,291878432,323987165,
     $325953177,319530131,428254030,300360972,317072973,323466190,
     $310748619,321267343,        0,439231643,304948251,302916702,
     $307209568,319825631,328115995,325887511,313210400,323987165,
     $325953177,319534294,313206293,321529490,323462733,319169867,
     $304456588,296133391,294134609,298328911,447423957,319432274,
     $321365517,317072715,        0,458204427,460334411,460333841,
     $327712768,443556758,443557728,443524639,330314646,300655768,
     $313271831,321595028,323528270,317072651,304456588,296133391,
     $294134609,298328911,447489495,319497812,321431054,314975499,
     $        0,460236444,325953308,328115935,321922464,309306461,
     $300753815,296330063,298230732,304456971,317072974,323495571/
      data (symbcd(j), j =    3649,  3762)/
     $321562134,315335895,304817108,298399136,311403677,302851031,
     $298427278,300299531,314975758,321398356,319488000,437265306,
     $464529181,323822932,308847759,304461466,311043217,304587787,
     $435070112,311436893,437200031,311404125,326018846,330301440,
     $447751327,305079324,302818391,309011862,323725016,328017693,
     $326084128,313537888,309306526,305013849,306947286,449521239,
     $323757786,326018719,319829206,300589907,294167310,296100875,
     $310748684,321300111,323561044,319464854,443229205,298427217,
     $296166284,302363915,317072909,321365587,319455232,460105367,
     $319464852,308946005,302719960,300786717,307209568,319825567,
     $326051612,327952084,323528206,314975435,302359436,296166223,
     $298329039,298267733,302752795,305046751,313538207,326018776,
     $323626577,317138252,308641792,451847627,432678932,        0,
     $432678932,        0,475144708,        0,432777239,432580625,
     $        0,456271201,307176475,298558290,296166281,300098564,
     $447784093,302818262,298361740,300131332,        0,443688226,
     $313501082,315303249,308716618,298033796,443688225,313402711,
     $310977743,304456583,        0,445654292,435070551,456041431,
     $        0,430285580,296133516,298165065,291733504,430351116/
      data (symbcd(j), j =    3763,  3876)/
     $296067980,296124416,449979271,460465351,462300891,328017755,
     $330180382,326084128,311436383,300852187,302818392,319432338,
     $435004505,319465044,323561103,321299980,312845387,298197837,
     $294101776,296264592,296189952,443556895,298722135,296362895,
     $302392523,312845836,323462868,325822108,319792480,309343327,
     $300819351,298460111,304493581,308684108,319206860,321365652,
     $323724892,317699614,313500895,302972928,437134493,313533771,
     $437134363,307111198,310748491,        0,432907164,300885023,
     $307242400,319792734,323888794,321660373,298169243,300786652,
     $302982303,315598366,321791578,319563157,296072076,325461707,
     $430286539,        0,435168928,309048288,300918367,456139927,
     $443295064,319530645,325658321,323429900,312845451,300295053,
     $296199055,441165143,319497875,449554005,323561105,321332620,
     $457713165,312878220,300327823,438707086,        0,451847627,
     $319141408,319141408,296232720,451847056,432580369,327680000,
     $435168151,437232600,435168864,321893407,321893336,307012953,
     $319563414,325691089,323429900,312845451,300295053,296199055,
     $432776151,304883032,319530644,449586774,323593873,321332620,
     $457713165,312878220,300327823,438707086,        0,454010461/
      data (symbcd(j), j =    3877,  3990)/
     $323921503,315630880,305112028,298558354,300360780,310748491,
     $319170190,325625554,323659287,313271576,304849877,456074655,
     $311403614,441426972,300655570,302458060,434644045,310781260,
     $319202960,449193550,323528338,321562007,457811478,313238807,
     $304817107,443261973,300482560,430974688,304460640,296724127,
     $458236939,304447488,441459679,298754971,300721176,306947478,
     $319465044,323561103,321299852,306586573,298296210,300557333,
     $306914711,319563353,323856029,321889696,307246111,300852187,
     $302818456,315336214,323626706,325559949,321267083,306553804,
     $298230607,296297364,302720151,315368985,321758813,319796830,
     $315597983,300888974,304494028,323420160,455812564,311010515,
     $302654358,296526682,298755103,309339424,317695581,323790484,
     $321365452,310748299,300295054,300360716,455910934,313144920,
     $317367572,308945941,298595476,300622745,298656733,307213211,
     $302982367,311403998,321762655,319727193,321529359,314979789,
     $310781068,300318720,449750412,317076893,317629900,432711637,
     $334115733,298461140,        0,432711637,334115733,298461140,
     $        0,466756356,295843748,334635844,        0,432842713,
     $334246809,298592216,432580561,333984657,298330064,        0/
      data (symbcd(j), j =    3991,  4104)/
     $445785250,303014811,296428370,298230793,306390276,312620324,
     $313664738,305112027,298525586,300328009,308487492,        0,
     $431104994,305112283,311108882,308716617,300098372,297939812,
     $298984482,307209499,313206098,310813833,302195588,        0,
     $441459807,308978836,441459860,441459935,304784532,430875549,
     $315336151,430876119,430875484,317466071,451847581,298558295,
     $451846999,451847644,296493911,        0,438707211,300262284,
     $298230734,302457933,304423944,298038221,300295180,302425037,
     $436577354,438707208,        0,434578317,298197963,302359628,
     $304522254,300364749,300295180,302425037,        0,443688135,
     $310621412,311567623,453944989,319792480,307241951,296657755,
     $298623960,317335059,321431119,319202636,306586637,300365341,
     $317662559,307209182,298754971,300721621,321496721,323462733,
     $319169867,306553804,296166350,455550348,        0,445653771,
     $445555531,293975325,325429003,445654795,434677329,432547472,
     $        0,433070987,435135436,433071520,321889950,325986009,
     $323724886,315274207,315598430,323888793,321627542,434840982,
     $321562260,325658319,323397196,314942347,434808213,321529490,
     $323462733,314975180,        0,462268125,321889760,309339231/
      data (symbcd(j), j =    4105,  4218)/
     $300852123,296493907,298329038,304489675,317040204,325527312,
     $462268123,323921502,317695199,305079259,298591123,300426317,
     $308684236,321300110,325592848,        0,433070987,435135436,
     $433071456,319792797,325953304,327788240,323429900,312845195,
     $435135839,319759965,323856088,325691024,321332749,312878028,
     $        0,433070987,435135436,433071776,435136159,324023254,
     $313206101,434808149,434513548,323335051,323321856,433070987,
     $435135435,298169248,324023263,323987104,434840918,313177045,
     $313163776,462268125,321889760,309339231,300852123,296493907,
     $298329038,304489675,317040204,325527312,327820756,462268123,
     $323921502,317695199,305079325,300786584,298427344,302457933,
     $308684236,321300110,325592787,317302228,        0,433070987,
     $433071072,300262283,462431968,325429003,462432011,434841302,
     $434808533,        0,433070987,300266400,300950475,        0,
     $449848720,312911052,304489421,298328912,449848800,317203853,
     $312878283,304456652,298230608,        0,433070987,300266400,
     $300950475,462431968,300562208,300528791,325429003,443262731,
     $        0,433070987,433071072,300299212,323364491,432383627,
     $        0,433070987,435004363,298169307,314946464,315045792/
      data (symbcd(j), j =    4219,  4332)/
     $315045723,314947419,329623435,466626443,        0,433070987,
     $435069899,298169309,327529376,325531360,325531360,328214283,
     $        0,443556959,300852123,296493907,298329038,304489675,
     $317040204,325527312,329885528,328050397,321889760,309343519,
     $305079259,298591123,300426317,310781324,321300176,327788312,
     $325953118,315598111,        0,433070987,435135435,298169248,
     $317728351,323954396,325887639,321594837,300594143,317695582,
     $323888793,321627606,300613632,443556959,300852123,296493907,
     $298329038,304489675,317040204,325527312,329885528,328050397,
     $321889760,309343519,305079259,298591123,300426317,310781324,
     $321300176,327788312,325953118,315598111,449259209,327464334,
     $317138697,        0,433070987,435135435,298169248,315631199,
     $323954396,325887639,321594773,300594143,315598430,323888793,
     $321627542,300627221,323331787,447391435,        0,460236383,
     $315630752,300917597,296592281,300688471,315270676,321496721,
     $323429965,314975372,302425038,296171229,321824286,315597983,
     $300884893,298689497,304883094,319465107,325625550,321267083,
     $306553804,296157184,441427083,443524299,306557728,321922655,
     $428876575,321880064,433070993,300360780,310748555,321267406/
      data (symbcd(j), j =    4333,  4446)/
     $327722784,433071072,300459022,304522508,314975821,323430097,
     $326117152,        0,428877067,428876640,310851360,326116622,
     $462431499,        0,428876939,428876640,306656736,306656733,
     $306558429,327529952,327628960,338700046,475014923,        0,
     $430974603,325432160,298854091,460334752,296072928,298165067,
     $        0,428877014,308651275,428876640,311113440,324019414,
     $460334358,310738944,458236747,460333963,430974688,430973791,
     $323990412,325461707,430286539,        0,455910987,323335769,
     $323790475,455812568,313304217,302785430,296330065,298263564,
     $306554187,317072974,455812440,306979863,300622739,298361806,
     $302425228,312878670,        0,433070987,300266400,300950475,
     $434840664,309110169,319563414,325691089,323429900,314942667,
     $304489422,434840792,315368983,321595027,323528270,319202700,
     $308683726,        0,455812568,313304217,302785430,296330065,
     $298263564,306554187,317072974,455812629,317433176,306979863,
     $300622739,298361806,302425228,312878541,319268430,        0,
     $456140363,323335776,324019851,455812568,313304217,302785430,
     $296330065,298263564,306554187,317072974,455812440,306979863,
     $300622739,298361806,302425228,312878670,        0,432612946/
      data (symbcd(j), j =    4447,  4560)/
     $321562135,317465945,307012632,298525523,296264590,302392459,
     $312845772,321336211,319399445,317433176,306979863,300622739,
     $298361806,302425228,312878541,319268430,        0,447751392,
     $305112092,302359627,447751519,309306462,441427036,304460633,
     $311207192,430744408,311164928,458008153,321201671,316876101,
     $308454470,302228359,458008202,321103301,312616068,302162823,
     $455812568,313304217,302785430,296330065,298263564,306554187,
     $317072974,455812440,306979863,300622739,298361806,302425228,
     $312878670,        0,433070987,300266400,300950475,434807960,
     $311207385,321660565,323335125,306947352,315368983,321562187,
     $323321856,433070943,296690589,300852254,303014880,298857375,
     $298787806,300917663,432841611,300266393,300721099,        0,
     $433070943,296690589,300852254,303014880,298857375,298787806,
     $300917663,432841604,300037017,300721092,        0,433070987,
     $300266400,300950475,458008153,300398233,300364946,319137419,
     $443131531,        0,433070987,300266400,300950475,        0,
     $432841611,300266393,300721099,434807960,311207385,321660565,
     $323335125,306947352,315368983,321562187,323335829,330049497,
     $340568344,346728779,457877335,334243928,342599957,344303947/
      data (symbcd(j), j =    4561,  4674)/
     $        0,432841611,300266393,300721099,434807960,311207385,
     $321660565,323335125,306947352,315368983,321562187,323321856,
     $441230360,298525523,296264590,302392459,312845772,321332881,
     $323593814,317465945,307016856,302752726,298427281,300360717,
     $306586956,317105678,321431123,319497687,313271448,        0,
     $432841604,300037017,300721092,434840664,309110169,319563414,
     $325691089,323429900,314942667,304489422,434840792,315368983,
     $321595027,323528270,319202700,308683726,        0,455910980,
     $323106393,323790468,455812568,313304217,302785430,296330065,
     $298263564,306554187,317072974,455812440,306979863,300622739,
     $298361806,302425228,312878670,        0,432841611,300266393,
     $300721099,434742294,306980121,317502419,302687383,311174616,
     $317489152,453715416,311207001,298591062,298460179,313042384,
     $449357263,317138316,451323148,304489357,434512782,296171030,
     $317400472,451650840,304882583,434906006,300561301,302654802,
     $317236751,319235532,310748235,298197838,        0,435168203,
     $302363616,303047691,428647641,309080857,294397144,        0,
     $432841615,300295243,310748556,321368985,300721103,302425228,
     $310781325,321369689,321234571,455911065,323321856,428647563/
      data (symbcd(j), j =    4675,  4711)/
     $428647257,306624025,317498509,453813387,        0,430744715,
     $430744473,306656665,306656662,306558358,323335577,323434457,
     $332179086,468493963,        0,430745099,321237849,298624587,
     $455910937,296072793,298165067,        0,428647563,428647257,
     $306624025,317498509,297940505,306553796,297926656,451683147,
     $455910348,430745177,430744408,317469644,321267275,430286411,
     $        0/
c
      data (istart(j), j=1,229)/ 
     $1,5,16,26,34,39,43,54,58,60,66,70,73,78,82,93,100,112,120,131,
     $134,140,143,148,151,154,296,305,314,322,331,340,344,355,360,364,
     $370,374,376,385,390,399,408,417,421,430,434,439,442,447,450,455,
     $3177,3186,3189,3197,3205,3208,3217,3229,3232,3247,3259,3262,3264,
     $3266,3269,3275,3281,3285,3290,3293,158,162,173,176,180,185,189,
     $193,205,207,211,214,219,223,227,238,242,249,253,256,265,275,278,
     $287,459,471,486,494,506,515,526,535,549,554,563,567,577,584,598,
     $607,613,623,632,636,644,655,662,672,683,690,710,726,740,749,757,
     $775,785,790,799,809,815,826,834,855,868,898,918,935,942,952,958,
     $967,975,983,1272,1290,1305,1319,1335,1350,1360,1388,1399,1406,
     $1417,1427,1432,1450,1461,1478,1494,1509,1519,1535,1542,1553,1559,
     $1568,1576,1585,3306,3325,3330,3351,3373,3378,3396,3419,3433,3462,
     $3485,3488,3490,3492,3495,3505,3515,3519,3523,3526,990,997,1017,
     $1023,1029,1038,1045,1055,1080,1085,1095,1101,1112,1120,1133,1154,
     $1162,1175,1183,1190,1205,1226,1234,1252,1592,1611,1637,1650,1671,
     $1686,1701,1716,1737,1744,1757,1767,1779/
      data(istart(j),j=230,433)/
     $1789,1810,1825,1834,1849,1865,1872,1887,1905,1916,1932,1953,1960,
     $1978,1995,2009,2018,2026,2046,2056,2061,2071,2081,2087,2098,2106,
     $2126,2138,2167,2185,2202,2209,2220,2226,2235,2243,2251,2522,2540,
     $2556,2568,2587,2600,2617,2637,2651,2663,2678,2693,2701,2725,2742,
     $2757,2776,2791,2803,2817,2825,2842,2855,2874,2894,2913,3546,3566,
     $3572,3592,3616,3620,3638,3660,3673,3702,3724,3727,3729,3731,3734,
     $3744,3754,3758,3762,3765,4074,4082,4102,4121,4136,4146,4154,4176,
     $4185,4189,4199,4208,4214,4224,4232,4252,4264,4287,4302,4323,4329,
     $4341,4347,4357,4364,4371,4379,4396,4413,4429,4446,4464,4474,4497,
     $4508,4519,4530,4539,4543,4562,4573,4591,4608,4625,4634,4656,4663,
     $4674,4680,4690,4697,4704,3784,3803,3809,3825,3846,3853,3876,3904,
     $3909,3941,3969,3976,3980,3984,3991,4003,4015,4031,4042,4050,2258,
     $2260,2262,2283,2289,2301,2305,2309,2320,2336,2360,2373,2377,2381,
     $2384,2391,2399,2402,2406,2415,2435,2454,2473,2500,2927,2932,2937,
     $2942,2964,2977,2983,2990,2997,3012,3027,3051,3056,3063,3070,3086,
     $3098,3100,3102,3104,3123,3130,3135,3154,  71/
      data (width(j), j=1,216)/
     $18,21,21,21,19,18,21,22, 8,16,21,17,24,22,22,21,22,21,20,16,22,
     $18,24,20,18,20, 19,19,18,19,18,12,19,19, 8,10,17,8,30,19,19,19,
     $19,13,17,12,19,16,22,17,16,17,20,20,20,20,20,20,20,20,20,20,26,
     $26,22,26,14,14, 16,10,10,20,18,21,17,18,19,20,22,22,8,21,18,24,
     $22,18,22,22,21,18,16,18,20,20,22,20,21,19,19,18,16,15,20,21,11,
     $18,16,21,18,16,17,22,18,20,20,20,22,18,23,23,20,22,21,22,21,20,
     $23,24,11,15,22,18,25,23,22,22,22,22,20,19,24,20,24,20,21,20,20,
     $21,19,21,19,13,19,22,11,11,21,11,33,22,20,21,20,17,17,15,22,18,
     $24,20,19,18,20,20,20,20,20,20,20,20,20,20,26,26,22,26,14,14,16,
     $10,10,20,20,22,18,20,21,20,24,22,11,22,20,25,23,22,22,24,22,21,
     $19,19,21,20,23,22/
      data (width(j), j= 217, 433)/
     $23,21,20,19,18,18,22,23,12,20,20,23,20,17,18,22,19,21,20,20,22,
     $18,23,23,20,24,21,23,23,22,22,26,13,18,23,20,27,25,22,23,22,24,
     $23,21,25,20,26,22,21,22,21,19,18,21,18,15,20,21,13,13,20,12,33,
     $23,18,21,20,17,17,14,23,20,29,20,21,20,21,21,21,21,21,21,21,21,
     $21,21,26,26,22,26,15,15,17,11,11,21,20,20,21,21,19,18,21,22,9,
     $17,21,17,24,22,22,20,22,20,20,17,22,20,26,20,19,20,20,20,18,20,
     $18,14,20,20,9,9,19,9,31,20,19,20,20,14,17,11,20,16,24,18,16,
     $18,20,20,20,20,20,20,20,20,20,20,25,25,23,25,14,14,16,11,11,19,
     $24,24,19,20,17,24,24,25,24,24,25,24,24,22,26,34,10,22,31,19,14,
     $14,27,22,14,14,21,16,16,10,10,10,18,24,25,11,11,11,21,24,14,14,
     $8,16,14,26,22,8, 18/
      data (ssymbc(j), j=1,120)/
     $             471149226,357246358,315959338, 68157440,470825002,
     $ 345320100,357443862,327886236,315762474,336920576,470825002,
     $ 355313115,336920576,470493226,449850016,0,455911911,456370649,0,
     $ 471149216,336274848,336922656,0,470493226,357574048,336920576,
     $ 449522346,315959958,0,470825002,355641947,336274907,317892650,0,
     $ 456370208,336279584,351502336,481470811,325953253,347256234,
     $ 326284694,325958294,346929184,357892096,449850016,470493226,
     $ 455911911,485271143,0,450177706,315304598,315949056,470493226,0,
     $ 470825002,355313115,336935525,336274917,355631104,470853600,
     $             336570464,336625664,468592477,328181537,330409956,
     $ 338831587,345024799,342796380,334364672,466265814,319563163,
     $ 313468258,315794984,326444971,341158250,353643173,359738078,
     $ 357411352,346761365,332038144,465905227,312910991,300491605,
     $ 292332190,290530023,297116654,307799411,322611126,341518837,
     $ 360295345,372714731,380874146,382676313,376089682,365406925,
     $ 350595210,331677696,468592477,328181537,330409956,338831587,
     $ 345024799,342796380,334378847,330344289,466560930,468625379,
     $ 470722595,472819811,474949794,477079777,0,462300964,345123100,
     $ 328087389,330413981,332511197,334608413,336705629,338802845/
      data (ssymbc(j), j=121,128)/
     $340900061,342982656,470623971,347187226,464594973,
     $342964256,334571552,338755584/
      data isstar /1,5,11,14,17,20,24,27,30,35,38,45,50,53,55,60,63,70,
     $ 81,98,113,123/
c
      end
c______________________________________________________________________
      subroutine dfault(init, code, image)
c$$$$ calls nothing
c  Set defaults to user preferences
c
c  The commands in the array  cmd  are run before plotxy reads from the
c  command line.  Up to  ncmd such initital commands can be set
c
      parameter (ncmd=20)
      character*116 image, code*4, cmd(ncmd)*40, bkslsh*1
      data cmd/ncmd*' '/, k/1/
c
      if (k.eq. 1) then
c  In command list back-slashes must be doubled in Unix systems, as
c  in:      cmd(1)='title \\comp\\ '.  Alternatively, you can use the
c  character variable  bkslsh:
        bkslsh=char(92)
c
c
c  Enter list of default options here:
        cmd(1)='weight 6 12'
        cmd(2)='char 0.08'
c
c
      endif
      code(1:4)=cmd(k)(1:4)
      image=cmd(k)(5:40)
      k=k+1
      if (cmd(k-1).eq. ' ' .or. k.gt. ncmd) init=0
      return
      end
c_______________________________________________________________________

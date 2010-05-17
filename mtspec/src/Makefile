# Makefile to compile the mtspec library, for access to
# any user in any machine. The folder containing this makefile
# will have all the files needed, including subroutines, 
# test programs, plotting routines, etc. 
#
# A single library file will be created and multiple modules
# will be created. But the important part is the compilation
# of all routines in a single library. 
#
# Included are among other things
#
# multitaper library
# plotting library
# splines library
#
# Last Modified:
# 	January 12, 2009
# 	German A. Prieto
#
#

SHELL = /bin/tcsh

#---------------------------------------------------------------------
# make.inc included
#--------------------------------------------------------------------

include make.inc

#---------------------------------------------------------------------
# Compiler
#--------------------------------------------------------------------

# gfortran Compiler
FC = gfortran

#--------------------------------------------------------------------
# Compiler flags
#--------------------------------------------------------------------

#   Optimize
FFLAGS = -O -fPIC
 
#--------------------------------------------------------------------
# Location of files
#--------------------------------------------------------------------

DIR     =   ./

# MTSPEC LIBRARY
MTSRC   =   ./mtspec/src/

# LAPACK LIBRARY
LASRC   =   ./lapack/src/

# GPLOT LIBRARY
PLSRC   =   ./gplot/src/

# FILTER LIBRARY
FLSRC =    ./filter/src/

# UNIX LIBRARY
UNSRC =    ./unix_cmd/src/

# STATS LIBRARY
STSRC =    ./stats/src/

# CONV2D LIBRARY
CVSRC =    ./conv2d/src/

# INVERSE LIBRARY
INSRC =    ./inverse/src/

# SEISMO LIBRARY
SESRC =    ./seismo/src/

# SPLINES LIBRARY
SPSRC =    ./splines/src/

# TIME-FREQUENCY LIBRARY
TFSRC =    ./time_freq/src/


#--------------------------------------------------------------------
# Module flag
#--------------------------------------------------------------------

# Intel or gfortran compiler
MFLAG = -I

MODULE = $(MFLAG)$(MOD)

#--------------------------------------------------------------------
# Fortran Files
#--------------------------------------------------------------------

# MTSPEC LIBRARY
MTFILES = spectra.f90      mtspec.f90      fft.f90          \
          ifft.f90         adaptspec.f90    \
          dpss.f90         dpss_ev.f90     eigenft.f90      \
          pythag.f90       sym_fft.f90     oct_spec.f90     \
          set_xint.f90     sft.f90         \
          xint.f90         qtdis.f90        \
          qsnorm.f90       yule.f90         \
	  wv_spec.f90     jackspec.f90     \
	  df_spec.f90      atanh2.f90      nearn.f90        \
          sine_psd.f90     sine_cohe.f90   dpss_spline.f90  \
	  mt_cohe.f90      nsqi.f90        qrfac.f90        \
	  rsm_eig.f90      rst_eig.f90	    \
	  nsinv.f90        ftest.f90       fdis.f90         \
	  psd_reshape.f90  qiinv.f90       zqrfac.f90       \
	  mt_deconv.f90    mt_transfer.f90 qi_nsqi.f90      \
          cumtrapz.f90     spec_gram.f90   \
          spec_gram2.f90   qi_nsqi2.f90    qi_nsqi3.f90

# LAPACK SUBROUTINES	  
LAPACK = dstevr.f lsame.f  xerbla.f  dcopy.f    dstebz.f  dstein.f   \
         dasum.f  daxpy.f  dlaebz.f  dstemr.f   dlarnv.f  idamax.f   \
         dscal.f  ddot.f   dlanst.f  ilaenv.f   dswap.f   dlamch.f   \
         dlagts.f dlagtf.f dnrm2.f   ieeeck.f   dsterf.f  dlascl.f   \
         dlapy2.f dlaev2.f iparmq.f  dlae2.f    disnan.f  dlassq.f   \
         dlasrt.f dlaruv.f dlarrc.f  dlaisnan.f dlarrv.f  dlarrr.f   \
         dlarre.f dlarrf.f dlarrj.f  dlarrb.f   dlarra.f  dlasq2.f   \
         dlaneg.f dlar1v.f dlarrk.f  dlarrd.f   dlasq3.f  dlaset.f   \
         dlasq4.f dlasq5.f dlasq6.f  dsyevr.f   dlansy.f  dormtr.f   \
         dsytrd.f dormqr.f dsytd2.f  dormql.f   dsyr2k.f  dlatrd.f   \
         dgemv.f  dorm2r.f dorm2l.f  dlarf.f    dsymv.f   dlarft.f   \
         dlarfg.f dlarfb.f dsyr2.f   dtrmv.f    iladlc.f  dtrmm.f    \
         dgemm.f  iladlr.f dger.f

# GPLOT LIBRARY
PLFILES = gplot.f90 gnuplot.f90

# SPLINES LIBRARY
SPFILES =  spline_cubic.f90

#--------------------------------------------------------------------
# Objects Files
#--------------------------------------------------------------------

# MTSPEC LIBRARY
MTOBJS =  $(MTFILES:.f90=.o)

# LAPACK LIBRARY
LAOBJS =  $(LAPACK:.f=.o)

# GPLOT LIBRARY
PLOBJS =  $(PLFILES:.f90=.o)

# SPLINES LIBRARY
SPOBJS =  $(SPFILES:.f90=.o)


#--------------------------------------------------------------------
# Compile libraries
#--------------------------------------------------------------------

# $@ means the target name
# $? means all dependencies (OBJS) that are new.
# $< similar to $? but just on dependency rules (%.o : %.f90)

lib/mtspec.so : $(PLOBJS) $(SPOBJS) $(MTOBJS)
	$(FC) $(FFLAGS) -shared -o mtspec.so -lfftw3 -llapack -L/usr/lib *.o
	mv mtspec.so lib/

lib/mtspec.dylib : $(PLOBJS) $(SPOBJS) $(MTOBJS)
	$(FC) $(FFLAGS) -shared -o mtspec.dylib -lfftw3 -llapack -L/usr/lib *.o
	mv mtspec.dylib lib/

all : libraries programs

libraries : libmtspec.a organize add_libs
 
libmtspec.a : $(LAOBJS) $(PLOBJS) $(SPOBJS)  \
               $(MTOBJS)
	ar cr $@ $(LAOBJS) $(PLOBJS) $(SPOBJS) \
                 $(MTOBJS) 
	ranlib $@

%.o : $(MTSRC)%.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o : $(LASRC)%.f
	$(FC) $(FFLAGS) -c $< -o $@

%.o : $(PLSRC)%.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o : $(SPSRC)%.f90
	$(FC) $(FFLAGS) -c $< -o $@

organize : libmtspec.a
	mv *.mod $(MOD)
	mv *.a $(LIB)

add_libs : libmtspec.a
	if (! -e tmp) mkdir tmp
	cd tmp && ar x $(FFTW3) 
	ar r $(LIB)libmtspec.a tmp/*.o
	rm -rf tmp

#--------------------------------------------------------------------
# Location of files
#--------------------------------------------------------------------

#MTSPEC PROGRAMS
MTPROG   =   ./mtspec/programs/
FIGPROG  =   ./mtspec/examples/src/
EXAMPLE  =   ./mtspec/examples/

#--------------------------------------------------------------------
# Compile programs
#--------------------------------------------------------------------

programs : mt_progs 

mt_progs : mtpsd wigner dual_freq coherence nsqi_psd mtpad trfunction deconv \
           figs

%:      $(MTPROG)%.f90 
	$(FC) $(FFLAGS) $(MODULE) $< -L$(LIB) -lmtspec \
	-o $(PROG)$@
	mv *.mod $(MOD) 

figs : fig1_2 fig3 fig4_5 fig6 

%:      $(FIGPROG)%.f90 
	$(FC) $(FFLAGS) $(MODULE) $< -L$(LIB) -lmtspec \
	-o $(EXAMPLE)$@

#--------------------------------------------------------------------
# Clean
#--------------------------------------------------------------------

clean:
	rm -f *.o
	rm -f $(LIB)libmtspec.a 
	rm -f mtspec.so
	rm -f mod/*
	rm -f bin/*




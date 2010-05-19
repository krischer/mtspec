# Locations

DIR =      ../
SRC = 	   ./src/

# Files

FILES = gplot.f90

# Objects

OBJS =  $(FILES:.f90=.o)

# Compiler

#FC = f95
#FC = g95
# Intel compiler
FC = ifort     

# Compiler flags
#   none
FFLAGS = -O
#   debug
#FFLAGS = -g 


# Compile

gplot.a : $(OBJS) plotxy
	ar cr $(DIR)$@ $(OBJS)
	ranlib $(DIR)$@
	mv *.mod $(DIR)
	rm *.o

%.o : $(SRC)%.f90
	$(FC) $(FFLAGS) -c $< -o $@

plotxy : plotprogram/plotxy.f
	$(FC) $(FFLAGS) $< -o $(DIR)plotxy

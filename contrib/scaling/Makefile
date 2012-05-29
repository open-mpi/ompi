PROGS = orte_no_op mpi_no_op mpi_barrier

all: $(PROGS)

CC = mpicc
CFLAGS = -g
CFLAGS_INTERNAL = -I../../.. -I../../../orte/include -I../../../opal/include
CXX = mpic++
CXXFLAGS = -g
F77 = mpif77
FC = mpif90
FFLAGS = -g

clean:
	rm -f $(PROGS) *~

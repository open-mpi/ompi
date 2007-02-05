PROGS = mpi_no_op hello hello_nodename abort multi_abort simple_spawn mpi_spin delayed_abort

all: $(PROGS)

CC = mpicc
CFLAGS = -g
CXX = mpic++
CXXFLAGS = -g
F77 = mpif77
FC = mpif77
FFLAGS = -g

clean:
	rm -f $(PROGS) *~

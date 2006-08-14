PROGS = no_op mpi_no_op hello hello_null sigusr_trap hello_nodename spin orte_nodename

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

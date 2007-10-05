PROGS = mpi_no_op hello hello_nodename abort multi_abort simple_spawn concurrent_spawn spawn_multiple mpi_spin delayed_abort loop_spawn loop_child bad_exit

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

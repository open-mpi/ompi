PROGS = mpi_no_op mpi_barrier hello hello_nodename abort multi_abort simple_spawn concurrent_spawn spawn_multiple mpi_spin delayed_abort loop_spawn loop_child bad_exit pubsub hello_barrier segv accept connect hello_output hello_show_help crisscross read_write

all: $(PROGS)

# These guys need additional -I flags

hello_output: hello_output.c
	$(CC) $(CFLAGS) $(CFLAGS_INTERNAL) $^ -o $@

hello_show_help: hello_show_help.c
	$(CC) $(CFLAGS) $(CFLAGS_INTERNAL) $^ -o $@

CC = mpicc
CFLAGS = -g
CFLAGS_INTERNAL = -I../../.. -I../../../orte/include -I../../../opal/include
CXX = mpic++
CXXFLAGS = -g
F77 = mpif77
FC = mpif77
FFLAGS = -g

clean:
	rm -f $(PROGS) *~

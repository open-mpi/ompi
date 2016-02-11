PROGS = mpi_no_op mpi_barrier hello hello_nodename abort multi_abort simple_spawn concurrent_spawn spawn_multiple mpi_spin delayed_abort loop_spawn loop_child bad_exit pubsub hello_barrier segv accept connect hello_output hello_show_help crisscross read_write ziatest slave reduce-hang ziaprobe ziatest bcast_loop parallel_w8 parallel_w64 parallel_r8 parallel_r64 sio sendrecv_blaster early_abort debugger singleton_client_server intercomm_create spawn_tree init-exit77 mpi_info info_spawn server client paccept pconnect ring hello.sapp

all: $(PROGS)

# These guys need additional -I flags

hello_output: hello_output.c
	$(CC) $(CFLAGS) $(CFLAGS_INTERNAL) $^ -o $@

hello_show_help: hello_show_help.c
	$(CC) $(CFLAGS) $(CFLAGS_INTERNAL) $^ -o $@

hello.sapp: hello.c myhello.spec
	$(CC) $(CFLAGS) $(CLAGS_INTERNAL) hello.c -o hello
	singularity build myhello.spec
	singularity install hello.sapp

CC = mpicc
CFLAGS = -g --openmpi:linkall
CFLAGS_INTERNAL = -I../../.. -I../../../orte/include -I../../../opal/include
CXX = mpic++ --openmpi:linkall
CXXFLAGS = -g
FC = mpifort -openmpi:linkall
FCFLAGS = -g

clean:
	rm -f $(PROGS) *~

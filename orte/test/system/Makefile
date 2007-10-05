PROGS = no_op sigusr_trap spin orte_nodename orte_proc_subscribe orte_spawn orte_loop_spawn orte_loop_child orte_abort get_limits orte_ring spawn_child

all: $(PROGS)

CC = ortecc
CFLAGS = -g
CXX = ortec++
CXXFLAGS = -g
FFLAGS = -g

clean:
	rm -f $(PROGS) *~

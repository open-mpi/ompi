PROGS = no_op sigusr_trap spin orte_nodename orte_spawn orte_loop_spawn orte_loop_child orte_abort get_limits orte_ring spawn_child orte_tool orte_no_op binom oob_stress iof_stress iof_delay radix orte_barrier opal_interface orte_spin segfault orte_exit orte_db orte_sensor test-time event-threads psm_keygen regex orte_errors evpri-test opal-evpri-test evpri-test2 mapper reducer opal_hotel orte_dfs orte_allocate

all: $(PROGS)

CC = ortecc
CFLAGS = -g
CXX = ortec++
CXXFLAGS = -g
FFLAGS = -g

clean:
	rm -f $(PROGS) *~

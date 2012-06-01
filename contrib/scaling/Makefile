PROGS = orte_no_op mpi_no_op

all: $(PROGS)

CC = mpicc
CFLAGS = -O
CFLAGS_INTERNAL = -I../../.. -I../../../orte/include -I../../../opal/include

clean:
	rm -f $(PROGS) *~

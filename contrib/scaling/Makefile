PROGS = orte_no_op mpi_no_op

all: $(PROGS)

CFLAGS = -O

orte_no_op:
	ortecc -o orte_no_op orte_no_op.c

mpi_no_op:
	mpicc -o mpi_no_op mpi_no_op.c

clean:
	rm -f $(PROGS) *~

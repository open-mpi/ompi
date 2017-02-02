PROGS = orte_no_op mpi_no_op mpi_memprobe

all: $(PROGS)

CFLAGS = -O

orte_no_op: orte_no_op.c
	ortecc -o orte_no_op orte_no_op.c

mpi_no_op: mpi_no_op.c
	mpicc -o mpi_no_op mpi_no_op.c

mpi_memprobe: mpi_memprobe.c
	mpicc -o mpi_memprobe mpi_memprobe.c -lopen-pal -lopen-rte

clean:
	rm -f $(PROGS) *~

#
# $HEADER$
#

PROGS = mpi-ping

all: $(PROGS)

CC = mpicc
CFLAGS = -g

clean:
	rm -f $(PROGS)

#mpi-ping-thread: mpi-ping-thread.c
#	$(CC) -pthread $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS) -lpthread

mpi-ping: mpi-ping.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

run-ping: mpi-ping
	mpirun -np 2 ./mpi-ping

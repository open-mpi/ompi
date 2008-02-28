PROGS = dss_buffer dss_payload dss_cmp dss_copy dss_print dss_release dss_set_get dss_size

all: $(PROGS)

CC = mpicc
CFLAGS = -g -I../../ -I../../opal/include

clean:
	rm -f $(PROGS) *~ .gdb*

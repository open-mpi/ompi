# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include  \
	-I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include  \
        -I../src  -I../..  -I../../tcp/base \
	-I/usr/lib/qsnet/elan4/include 

LDFLAGS += -L$(prefix)/lib -L../src/.libs 

EXECS = qsnet_init qsnet_qdma qsnet_rdma mpitest

units: $(EXECS)

qsnet_init: qsnet_init.c
	$(CC) -g $(AM_CPPFLAGS) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS) -lmpi -pthread -lm -lmca_ptl_elan

qsnet_qdma: qsnet_qdma.c
	$(CC) -g $(AM_CPPFLAGS) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS) -lmpi -pthread -lm -lmca_ptl_elan

qsnet_rdma: qsnet_rdma.c
	$(CC) -g $(AM_CPPFLAGS) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS) -lmpi -pthread -lm -lmca_ptl_elan

mpitest: mpi_test.c
	$(CC) -I${HOME}/installs/openmpi/include -L${HOME}/installs/openmpi/lib -o mpitest mpi_test.c -lmpi

clean:
	-rm -rf $(EXECS) 

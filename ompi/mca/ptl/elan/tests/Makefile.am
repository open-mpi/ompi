# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include  \
	-I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include  \
        -I../src  -I../..  -I../../tcp/base \
	-I/usr/lib/qsnet/elan4/include 

LDFLAGS += -L$(prefix)/lib -L../src/.libs 

EXECS = qsnet_init qsnet_qdma qsnet_rdma mpitest lat check init_elan bw check_bw

units: $(EXECS)

qsnet_init: qsnet_init.c
	$(CC) -g $(AM_CPPFLAGS) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS) -lmpi -pthread -lm -lmca_ptl_elan

qsnet_qdma: qsnet_qdma.c
	$(CC) -g $(AM_CPPFLAGS) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS) -lmpi -pthread -lm -lmca_ptl_elan

qsnet_rdma: qsnet_rdma.c
	$(CC) -g $(AM_CPPFLAGS) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS) -lmpi -pthread -lm -lmca_ptl_elan

mpitest: mpi_test.c
	${HOME}/installs/openmpi/bin/mpicc -g -o mpitest mpi_test.c 

check_bw: %: %.c
	${HOME}/installs/openmpi/bin/mpicc -g -o $@ check_bw.c

check: %: %.c
	${HOME}/installs/openmpi/bin/mpicc -g -o $@ check.c

bw: %: %.c
	${HOME}/installs/openmpi/bin/mpicc -g -o bw bw.c

lat: %: %.c
	${HOME}/installs/openmpi/bin/mpicc -g -o lat lat.c

init_elan: %: %.c
	gcc -g $< -o $@ -lelan4 -lelan -lrmscall -lelan3

clean:
	-rm -rf $(EXECS) 

#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

EXTRA_DIST = $(pkgdata_DATA)
pkgdata_DATA = help-mpirun.txt

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = mpirun

mpirun_SOURCES = \
        mpirun.c 

mpirun_LDADD = $(libs) 
mpirun_DEPENDENCIES = $(mpirun_LDADD)

# JMS Temporary hack for those who can't help themselves from typing
# "mpirun2" :-)
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun2; ln -s mpirun mpirun2)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun2

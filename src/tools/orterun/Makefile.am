#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

EXTRA_DIST = $(pkgdata_DATA)
pkgdata_DATA = help-orterun.txt

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = orterun

AM_CPPFLAGS = -DOMPI_VERSION=\"$(OMPI_VERSION)\"

orterun_SOURCES = \
        orterun.c 

orterun_LDADD = $(libs) 
orterun_DEPENDENCIES = $(libs)

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun; ln -s orterun mpirun)
	(cd $(DESTDIR)$(bindir); rm -f mpiexec; ln -s orterun mpiexec)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun \
		$(DESTDIR)$(bindir)/mpiexec

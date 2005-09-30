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

dist_pkgdata_DATA = help-orterun.txt

# This is not quite in the Automake spirit, but we have to do it.
# Since the totalview portion of the library must be built with -g, we
# must eliminate the CFLAGS that are passed in here by default (which
# may already have debugging and/or optimization flags).  We use
# post-processed forms of the CFLAGS in the library targets down
# below.

CFLAGS = $(CFLAGS_WITHOUT_OPTFLAGS) $(TOTALVIEW_DEBUG_FLAGS)

libs = \
        $(top_builddir)/orte/liborte.la

bin_PROGRAMS = orterun

orterun_SOURCES = \
        main.c \
        orterun.c \
        orterun.h \
        totalview.c \
        totalview.h 

orterun_LDADD = $(libs) 
orterun_DEPENDENCIES = $(libs)

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun; ln -s orterun mpirun)
	(cd $(DESTDIR)$(bindir); rm -f mpiexec; ln -s orterun mpiexec)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun \
		$(DESTDIR)$(bindir)/mpiexec

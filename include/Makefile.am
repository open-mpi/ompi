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

dist_include_HEADERS = \
	orte_config.h \
	ompi_config_bottom.h \
        ompi_stdint.h \
	mpi.h

nodist_include_HEADERS = \
	ompi_config.h \
	mpif.h

# Add a hook to run *after* the file ompi_config.h has been installed
# out to the target location.  It changes the pesky PACKAGE_* macros
# that autoconf automatically generates (and there is no way of
# turning off) into OMPI_MPI_PACKAGE_* in order to make <mpi.h> safe to
# include with other files.

install-data-hook:
	sed -e 's/define PACKAGE/define OMPI_MPI_PACKAGE/' \
		$(DESTDIR)$(includedir)/ompi_config.h \
		> $(DESTDIR)$(includedir)/ompi_config.h.install
	cp $(DESTDIR)$(includedir)/ompi_config.h.install \
		$(DESTDIR)$(includedir)/ompi_config.h
	rm -f $(DESTDIR)$(includedir)/ompi_config.h.install
	chmod 0644 $(DESTDIR)$(includedir)/ompi_config.h

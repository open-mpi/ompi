# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

include_HEADERS = \
	ompi_config.h \
	ompi_config_bottom.h \
	mpi.h \
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
	mv $(DESTDIR)$(includedir)/ompi_config.h.install \
		$(DESTDIR)$(includedir)/ompi_config.h

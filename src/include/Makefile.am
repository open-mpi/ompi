# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_HEADERS	= \
		communicator.h \
		datatype.h \
		group.h \
		lam_constants.h \
		lam.h \
		lam_stdint.h \
		lam_types.h \
		proc.h \
		totalview.h

include_HEADERS	= \
		lam_config.h \
		lam_config_bottom.h \
		lam_stdint.h \
		mpi.h \
		mpif.h

# Add a hook to run *after* the file lam_config.h has been installed
# out to the target location.  It changes the pesky PACKAGE_* macros
# that autoconf automatically generates (and there is no way of
# turning off) into LAM_MPI_PACKAGE_* in order to make <mpi.h> safe to
# include with other files.

install-data-hook:
	sed -e 's/define PACKAGE/define LAM_MPI_PACKAGE/' \
		$(DESTDIR)$(includedir)/lam_config.h \
		> $(DESTDIR)$(includedir)/lam_config.h.install
	mv $(DESTDIR)$(includedir)/lam_config.h.install \
		$(DESTDIR)$(includedir)/lam_config.h

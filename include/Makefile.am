# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

include_HEADERS = \
	lam_config.h \
	lam_config_bottom.h \
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

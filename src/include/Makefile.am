# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

headers = \
	communicator.h \
	datatype.h \
	group.h \
	lam_constants.h \
	lam.h \
	lam_stdint.h \
	lam_types.h \
	proc.h \
	totalview.h

include_HEADERS = \
	lam_config.h \
	lam_config_bottom.h \
	mpi.h \
	mpif.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/include
lam_HEADERS = $(headers) $(include_HEADERS)
else
lamdir = $(includedir)
endif

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

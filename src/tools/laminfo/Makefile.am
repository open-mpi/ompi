# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS     = \
            -DLAM_PREFIX="\"$(prefix)\"" \
            -DLAM_BINDIR="\"$(bindir)\"" \
            -DLAM_LIBDIR="\"$(libdir)\"" \
            -DLAM_INCDIR="\"$(includedir)\"" \
            -DLAM_PKGLIBDIR="\"$(pkglibdir)\"" \
            -DLAM_SYSCONFDIR="\"$(sysconfdir)\"" \
            -DLAM_CONFIGURE_USER="\"@LAM_CONFIGURE_USER@\"" \
            -DLAM_CONFIGURE_HOST="\"@LAM_CONFIGURE_HOST@\"" \
            -DLAM_CONFIGURE_DATE="\"@LAM_CONFIGURE_DATE@\""

base            = $(top_builddir)/src
if WANT_SINGLE_MPI_LIBRARY
liblam          = 
libmpi          = $(base)/libmpi.la
else
liblam          = $(base)/lam/liblam.la
libmpi          = $(base)/mpi/libmpi.la
endif
libs            = $(libmpi) $(liblam) 

bin_PROGRAMS        = laminfo
laminfo_SOURCES     = laminfo.cc
laminfo_LDADD       = $(libs) $(LIBMPI_EXTRA_LIBS) $(LIBLAM_EXTRA_LIBS)
laminfo_LDFLAGS     = $(LIBMPI_EXTRA_LDFLAGS) $(LIBLAM_EXTRA_LDFLAGS)
laminfo_DEPENDENCIES    = $(libs)

clean-local:
	test -z "$(LAM_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(LAM_CXX_TEMPLATE_REPOSITORY)

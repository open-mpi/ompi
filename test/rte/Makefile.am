# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = \
            -I$(top_srcdir)/test/support \
            -DLAM_PREFIX="\"$(prefix)\"" \
            -DLAM_BINDIR="\"$(bindir)\"" \
            -DLAM_LIBDIR="\"$(libdir)\"" \
            -DLAM_INCDIR="\"$(includedir)\"" \
            -DLAM_PKGLIBDIR="\"$(pkglibdir)\"" \
            -DLAM_SYSCONFDIR="\"$(sysconfdir)\"" \
            -DLAM_CONFIGURE_USER="\"@LAM_CONFIGURE_USER@\"" \
            -DLAM_CONFIGURE_HOST="\"@LAM_CONFIGURE_HOST@\"" \
            -DLAM_CONFIGURE_DATE="\"@LAM_CONFIGURE_DATE@\""

bin_PROGRAMS = \
        openmpi

openmpi_SOURCES = \
        ompi_session_dir.c

openmpi_LDADD = \
       $(top_builddir)/src/mca/pcm/base/pcm_base_open.lo \
       $(top_builddir)/test/support/libsupport.la \
       $(top_builddir)/src/libmpi.la

openmpi_DFLAGS = $(LIBMPI_EXTRA_LDFLAGS) $(LIBLAM_EXTRA_LDFLAGS)
openmpi_DEPENDENCIES = $(libs) \
                       $(openmpi_LDADD)

clean-local:
	test -z "$(LAM_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(LAM_CXX_TEMPLATE_REPOSITORY)

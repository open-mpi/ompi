# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
            -DLAM_PREFIX="\"$(prefix)\"" \
            -DLAM_BINDIR="\"$(bindir)\"" \
            -DLAM_LIBDIR="\"$(libdir)\"" \
            -DLAM_INCDIR="\"$(includedir)\"" \
            -DLAM_PKGLIBDIR="\"$(pkglibdir)\"" \
            -DLAM_SYSCONFDIR="\"$(sysconfdir)\"" \
            -DLAM_CONFIGURE_USER="\"@LAM_CONFIGURE_USER@\"" \
            -DLAM_CONFIGURE_HOST="\"@LAM_CONFIGURE_HOST@\"" \
            -DLAM_CONFIGURE_DATE="\"@LAM_CONFIGURE_DATE@\""

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = \
        registry

registry_SOURCES = \
        registry.h \
        registry.c

registry_LDADD = \
        $(libs) \
        $(LIBMPI_EXTRA_LIBS) \
        $(LIBLAM_EXTRA_LIBS)

registry__DFLAGS = $(LIBMPI_EXTRA_LDFLAGS) $(LIBLAM_EXTRA_LDFLAGS)
registry_DEPENDENCIES = $(libs) \
                       $(registry_LDADD)

clean-local:
	test -z "$(LAM_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(LAM_CXX_TEMPLATE_REPOSITORY)

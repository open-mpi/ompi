# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
            -DOMPI_PREFIX="\"$(prefix)\"" \
            -DOMPI_BINDIR="\"$(bindir)\"" \
            -DOMPI_LIBDIR="\"$(libdir)\"" \
            -DOMPI_INCDIR="\"$(includedir)\"" \
            -DOMPI_PKGLIBDIR="\"$(pkglibdir)\"" \
            -DOMPI_SYSCONFDIR="\"$(sysconfdir)\"" \
            -DOMPI_CONFIGURE_USER="\"@OMPI_CONFIGURE_USER@\"" \
            -DOMPI_CONFIGURE_HOST="\"@OMPI_CONFIGURE_HOST@\"" \
            -DOMPI_CONFIGURE_DATE="\"@OMPI_CONFIGURE_DATE@\""

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = \
        registry

registry_SOURCES = \
        registry.h \
        registry.c

registry_LDADD = \
        $(libs) \
        $(LIBMPI_EXTRA_LIBS) \
        $(LIBOMPI_EXTRA_LIBS)

registry__DFLAGS = $(LIBMPI_EXTRA_LDFLAGS) $(LIBOMPI_EXTRA_LDFLAGS)
registry_DEPENDENCIES = $(libs) \
                       $(registry_LDADD)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

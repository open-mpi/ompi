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

bin_PROGRAMS = ompi_info
ompi_info_SOURCES = \
        ompi_info.h \
        ompi_info.cc \
        modules.cc \
        output.cc \
        param.cc \
        version.cc 
ompi_info_LDADD = $(libs) $(LIBMPI_EXTRA_LIBS) $(LIBOMPI_EXTRA_LIBS)
ompi_info_LDFLAGS = $(LIBMPI_EXTRA_LDFLAGS) $(LIBOMPI_EXTRA_LDFLAGS)
ompi_info_DEPENDENCIES = $(libs)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

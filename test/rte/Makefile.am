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
        openmpi

openmpi_SOURCES = \
        ompi_session_dir.c

bogus_sources = \
        openmpi.h \
        ompi_init.h \
        ompi_init.c \
        openmpi.c 

openmpi_LDADD = \
        $(libs) \
        $(LIBMPI_EXTRA_LIBS) \
        $(LIBLAM_EXTRA_LIBS) \
        $(top_builddir)/src/util/os_path.lo \
        $(top_builddir)/src/util/os_create_dirpath.lo \
        $(top_builddir)/src/util/sys_info.lo

openmpi_DFLAGS = $(LIBMPI_EXTRA_LDFLAGS) $(LIBLAM_EXTRA_LDFLAGS)
openmpi_DEPENDENCIES = $(libs) \
                       $(openmpi_LDADD)

clean-local:
	test -z "$(LAM_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(LAM_CXX_TEMPLATE_REPOSITORY)

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
        openmpi

openmpi_SOURCES = \
        openmpi.h \
        openmpi.c 

openmpi_LDADD = \
        $(libs) \
        $(LIBMPI_EXTRA_LIBS) \
        $(LIBOMPI_EXTRA_LIBS) \
        $(top_builddir)/src/util/os_path.lo \
        $(top_builddir)/src/util/os_create_dirpath.lo \
        $(top_builddir)/src/util/session_dir.lo \
        $(top_builddir)/src/util/proc_info.lo \
        $(top_builddir)/src/util/cmd_line.lo \
        $(top_builddir)/src/util/common_cmd_line.lo \
        $(top_builddir)/src/runtime/universe_init.lo \
        $(top_builddir)/src/runtime/universe_connect.lo \
        $(top_builddir)/src/util/sys_info.lo

openmpi_DFLAGS = $(LIBMPI_EXTRA_LDFLAGS) $(LIBOMPI_EXTRA_LDFLAGS)
openmpi_DEPENDENCIES = $(libs) \
                       $(openmpi_LDADD)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

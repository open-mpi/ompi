#
# Copyright (c) 2018      DataDirect Networks. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_fs_ime_DSO
component_noinst =
component_install = mca_fs_ime.la
else
component_noinst = libmca_fs_ime.la
component_install =
endif

# Source files

fs_ime_sources = \
        fs_ime.h \
        fs_ime.c \
        fs_ime_component.c \
        fs_ime_file_open.c \
        fs_ime_file_close.c \
        fs_ime_file_delete.c \
        fs_ime_file_sync.c \
        fs_ime_file_set_size.c \
        fs_ime_file_get_size.c

AM_CPPFLAGS = $(fs_ime_CPPFLAGS)

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fs_ime_la_SOURCES = $(fs_ime_sources)
mca_fs_ime_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(fs_ime_LIBS)
mca_fs_ime_la_LDFLAGS = -module -avoid-version $(fs_ime_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_fs_ime_la_SOURCES = $(fs_ime_sources)
libmca_fs_ime_la_LIBADD =  $(fs_ime_LIBS)
libmca_fs_ime_la_LDFLAGS = -module -avoid-version $(fs_ime_LDFLAGS)
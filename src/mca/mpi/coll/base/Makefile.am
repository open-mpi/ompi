#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_coll_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

headers = \
        base.h \
        coll_tags.h

libmca_mpi_coll_base_la_SOURCES = \
        coll_base_close.c \
        coll_base_open.c \
        coll_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/coll/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

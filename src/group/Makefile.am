# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libgroup.la

# Source code files

headers = \
        group.h

libgroup_la_SOURCES = \
	$(headers) \
        mpi_group_init.c \
        mpi_set_group_rank.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/group
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

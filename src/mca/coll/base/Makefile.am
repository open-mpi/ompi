#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_coll_base.la

headers = \
        base.h \
        coll_tags.h

libmca_coll_base_la_SOURCES = \
        coll_base_close.c \
        coll_base_comm_select.c \
        coll_base_comm_unselect.c \
	coll_base_find_available.c \
        coll_base_open.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/coll/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

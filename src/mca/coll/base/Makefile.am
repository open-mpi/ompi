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
	coll_base_comm_init.c \
        coll_base_open.c \
        coll_base_select.c \
	coll_base_query.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/coll/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

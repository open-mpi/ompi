#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_oob_base.la

# Source code files

headers = \
        base.h

# Library

libmca_oob_base_la_SOURCES = \
        $(headers) \
        oob_base_close.c \
        oob_base_open.c \
        oob_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/oob/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

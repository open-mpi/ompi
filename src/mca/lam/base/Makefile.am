#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_lam_base.la

# Source code files

headers = \
        param.h \
        module_exchange.h

# Library

libmca_lam_base_la_SOURCES = \
        mca_lam_param.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

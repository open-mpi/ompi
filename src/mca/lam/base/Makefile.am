#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_lam_base.la

# Source code files

headers = \
        mca_base_close.h \
        mca_base_module_compare.h \
        mca_base_open.h \
        mca_base_param.h \
        mca_base_module_exchange.h

# Library

libmca_lam_base_la_SOURCES = \
        $(headers) \
        mca_base_close.c \
        mca_base_cmd_line.c \
        mca_base_module_compare.c \
        mca_base_module_find.c \
        mca_base_module_registry.c \
        mca_base_open.c \
        mca_base_param.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

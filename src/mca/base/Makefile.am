#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DLAM_PKGLIBDIR=\"$(pkglibdir)\"

noinst_LTLIBRARIES = libmca_base.la

# Source code files

headers = \
        base.h \
        mca_base_module_exchange.h \
        mca_base_param.h

# Library

libmca_base_la_SOURCES = \
        $(headers) \
        mca_base_close.c \
        mca_base_cmd_line.c \
        mca_base_module_compare.c \
        mca_base_module_exchange.c \
        mca_base_module_find.c \
	mca_base_init_select_modules.c \
        mca_base_module_registry.c \
        mca_base_modules_open.c \
        mca_base_modules_close.c \
        mca_base_open.c \
        mca_base_param.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

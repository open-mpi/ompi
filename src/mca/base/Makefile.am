#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# Need this so that the base knows where to load dynamic components from
# (by default)

AM_CPPFLAGS = -DOMPI_PKGLIBDIR=\"$(pkglibdir)\"

noinst_LTLIBRARIES = libmca_base.la

# Source code files

headers = \
        base.h \
        mca_base_module_exchange.h \
        mca_base_msgbuf.h \
        mca_base_msgbuf_internal.h \
        mca_base_param.h \
        mca_base_param_internal.h

# Library

libmca_base_la_SOURCES = \
        $(headers) \
        mca_base_close.c \
        mca_base_cmd_line.c \
        mca_base_component_compare.c \
        mca_base_component_find.c \
        mca_base_component_repository.c \
        mca_base_components_open.c \
        mca_base_components_close.c \
	mca_base_init_select_components.c \
        mca_base_list.c \
        mca_base_module_exchange.c \
        mca_base_msgbuf.c \
        mca_base_open.c \
        mca_base_param.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# Need this so that the base knows where to load dynamic components from
# (by default)

AM_CPPFLAGS = \
        -DOMPI_PKGLIBDIR=\"$(pkglibdir)\" \
        -DOMPI_SYSCONFDIR=\"$(sysconfdir)\"
AM_LFLAGS = -Pmca_base_yy
LEX_OUTPUT_ROOT = lex.mca_base_yy

noinst_LTLIBRARIES = libmca_base.la

pkgdata_DATA = help-mca-base.txt

# Source code files

headers = \
        base.h \
        mca_base_module_exchange.h \
        mca_base_msgbuf.h \
        mca_base_msgbuf_internal.h \
        mca_base_param.h \
        mca_base_param_internal.h \
        mca_base_parse_paramfile_lex.h

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
        mca_base_param.c \
        mca_base_parse_paramfile.c \
        mca_base_parse_paramfile_lex.l


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_llm_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src
AM_LFLAGS = -Pmca_llm_base_yy
LEX_OUTPUT_ROOT = lex.mca_llm_base_yy

# Source code files

noinst_PROGRAMS = \
	hostparse
hostparse_SOURCES = \
	hostparse.c
hostparse_LDADD = \
	libmca_llm_base.la

headers = \
	base.h \
	base_internal.h \
	llm_base_parse_hostfile_lex.h

libmca_llm_base_la_SOURCES = \
        $(headers) \
	llm_base_map.c \
	llm_base_collapse.c \
	llm_base_close.c \
	llm_base_open.c \
	llm_base_parse_hostfile.c \
	llm_base_parse_hostfile_lex.l \
	llm_base_select.c \
	llm_base_util.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/llm/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


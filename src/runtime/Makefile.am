# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_builddir)/src/event

noinst_LTLIBRARIES = libruntime.la

# Source code files

headers = \
    runtime.h \
    runtime_types.h \
    universe_connect.h \
    ompi_progress.h

libruntime_la_SOURCES = \
	$(headers) \
    ompi_abort.c \
    ompi_finalize.c \
    ompi_init.c \
    ompi_universe.c \
    ompi_progress.c \
    ompi_rte_finalize.c \
    ompi_rte_init_stage1.c \
    ompi_rte_init_stage2.c \
    ompi_rte_llm.c \
    ompi_rte_monitor.c \
    ompi_rte_cmd_line_setup.c \
    ompi_rte_universe_exists.c \
    ompi_rte_parse_seed_cmd_line.c \
    ompi_rte_pcm.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/runtime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

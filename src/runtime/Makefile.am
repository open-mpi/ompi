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
    runtime_internal.h \
    ompi_progress.h \
    ompi_rte_wait.h

libruntime_la_SOURCES = \
	$(headers) \
    ompi_abort.c \
    ompi_finalize.c \
    ompi_init.c \
    ompi_progress.c \
    ompi_rte_finalize.c \
    ompi_rte_init.c \
    ompi_rte_init_cleanup.c \
    ompi_rte_llm.c \
    ompi_rte_monitor.c \
    ompi_rte_job_startup.c \
    ompi_rte_job_shutdown.c \
    ompi_rte_wait_startup_shutdown_msg.c \
    ompi_rte_process_status.c \
    ompi_rte_cmd_line_setup.c \
    ompi_vm_register.c \
    universe_exists.c \
    ompi_rte_parse_environ.c \
    ompi_rte_parse_cmd_line.c \
    ompi_rte_parse_daemon_cmd_line.c \
    ompi_rte_pcm.c \
    ompi_rte_wait.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/runtime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libcommunicator.la

# Source code files

headers = \
        communicator.h

libcommunicator_la_SOURCES = \
        $(headers) \
        lam_comm_init.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/communicator
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

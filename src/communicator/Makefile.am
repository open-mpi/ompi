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
        comm_init.c \
	comm.c \
	comm_cid.c \
	comm_publish.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/communicator
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

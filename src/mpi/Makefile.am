# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = communicator datatype interface runtime

noinst_LTLIBRARIES = libmpi_convenience.la

sources = \
        datatype/libmpi_datatype.la \
        interface/libmpi_interface.la \
        request/libmpi_request.la \
        runtime/libmpi_runtime.la

libmpi_convenience_la_SOURCES =
libmpi_convenience_la_LIBADD = $(sources)

# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = communicator datatype interface proc request runtime

noinst_LTLIBRARIES = libmpi_convenience.la

sources = \
        communicator/libmpi_communicator.la \
        datatype/libmpi_datatype.la \
        info/libmpi_info.la \
        interface/libmpi_interface.la \
        proc/libmpi_proc.la \
        request/libmpi_request.la \
        runtime/libmpi_runtime.la

libmpi_convenience_la_SOURCES =
libmpi_convenience_la_LIBADD = $(sources)

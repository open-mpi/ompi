# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = attribute communicator datatype group info interface proc request runtime win

noinst_LTLIBRARIES = libmpi_convenience.la

sources = \
	attribute/libmpi_attribute.la \
        communicator/libmpi_communicator.la \
        datatype/libmpi_datatype.la \
        group/libmpi_group.la \
        info/libmpi_info.la \
        interface/libmpi_interface.la \
        proc/libmpi_proc.la \
        request/libmpi_request.la \
        runtime/libmpi_runtime.la \
	win/libmpi_win.la

libmpi_convenience_la_SOURCES =
libmpi_convenience_la_LIBADD = $(sources)

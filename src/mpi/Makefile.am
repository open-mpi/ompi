# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS		= communicator datatype p2p

lib_LTLIBRARIES	= libmpi.la

libmpi_la_SOURCES	=
libmpi_la_LIBADD	= \
	communicator/libcommunicator.la \
	datatype/libdatatype.la \
	p2p/libp2p.la

# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# JMS: Need more stuff here:
# - Do we want a separate compile for profiling (affects SUBDIRS and LIBADD)

SUBDIRS			= profile

lib_LTLIBRARIES		= libmpi_f77.la

libmpi_f77_la_SOURCES	= \
			comm_get_name_f.c \
			comm_set_name_f.c \
			datatype_get_name_f.c \
			datatype_set_name_f.c

libmpi_f77_la_LIBADD	= profile/libmpi_f77_profile.la

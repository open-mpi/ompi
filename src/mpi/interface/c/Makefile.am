# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# JMS: Need more stuff here:
# - Do we want a separate compile for profiling (affects SUBDIRS and LIBADD)

SUBDIRS			= profile

lib_LTLIBRARIES		= libmpi_c.la

libmpi_c_la_SOURCES	= \
			c_get_name.c \
			c_set_name.c \
			d_get_name.c \
			d_set_name.c
libmpi_c_la_LIBADD	= profile/libmpi_c_profile.la

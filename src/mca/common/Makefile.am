#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = $(MCA_common_STATIC_SUBDIRS)
DIST_SUBDIRS = $(MCA_common_ALL_SUBDIRS)

noinst_LTLIBRARIES = libmca_common.la
libmca_common_la_SOURCES = mca_common_dummy.c
libmca_common_la_LIBADD = $(MCA_common_STATIC_LTLIBS)
libmca_common_la_DEPENDENCIES = $(libmca_common_la_LIBADD)


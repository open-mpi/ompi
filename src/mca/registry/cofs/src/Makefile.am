#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_registry_cofs.la
libmca_registry_cofs_la_SOURCES = \
	registry_cofs.h \
	registry_cofs_module.c \
	registry_cofs.c


#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
    -I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_pcm_wmi.la
libmca_pcm_wmi_la_SOURCES = \
	pcm_wmi.c \
	pcm_wmi.h \
	pcm_wmi_component.c

#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_pcm_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h \
	base_job_track.h

# Library

libmca_pcm_base_la_SOURCES = \
        $(headers) \
        pcm_base_close.c \
	pcm_base_comm.c \
	pcm_base_ioexecvp.c \
	pcm_base_job_track.c \
        pcm_base_open.c \
        pcm_base_select.c \
	pcm_base_util.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/pcm/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

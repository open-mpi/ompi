#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
            -DOMPI_SYSCONFDIR="\"$(sysconfdir)\""

noinst_LTLIBRARIES = libmca_llm_hostfile.la
libmca_llm_hostfile_la_SOURCES = \
	llm_hostfile.h \
	llm_hostfile_component.c \
	llm_hostfile_allocate.c \
	llm_hostfile_deallocate.c

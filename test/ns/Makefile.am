# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	ns

ns_SOURCES = ns.c
ns_LDADD = \
        $(top_builddir)/src/ns/name_server.lo \
        $(top_builddir)/test/support/libsupport.la 
ns_DEPENDENCIES = $(ompi_ns_LDADD)


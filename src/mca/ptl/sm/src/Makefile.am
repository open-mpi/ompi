#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_ptl_sm.la
libmca_ptl_sm_la_LIBADD = \
        $(top_ompi_builddir)/src/mca/common/sm/libmca_common_sm.la
libmca_ptl_sm_la_SOURCES = \
	ptl_sm.c \
	ptl_sm.h \
	ptl_sm_component.c \
	ptl_sm_sendreq.c \
	ptl_sm_sendfrag.c \
	ptl_sm_recvfrag.c

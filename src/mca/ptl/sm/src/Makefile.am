#
# $HEADER$
#

# A word of explanation...
#
# We link this component to common/sm/libmca_common_sm.la. This is
# because all shared-memory based components (e.g., mpool, ptl, etc.)
# need to share some common code and data.  There's two cases:
#
# 1. libmca_common_sm.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
#
# 2. libmca_common_sm.la is a static library.  In this case, it will
# be rolled up into the top-level libmpi.la.  It will also be rolled
# into each component, but then the component will also be rolled up
# into the upper-level libmpi.la.  Linkers universally know how to
# "figure this out" so that we end up with only one copy of the code
# and data.

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

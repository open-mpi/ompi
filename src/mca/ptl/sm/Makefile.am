#
# $HEADER$
#

# Use the top-level OMPI Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = src

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# libompi_ssi_coll_ompi_basic.la

if OMPI_BUILD_ptl_sm_LOADABLE_MODULE
module_noinst =
module_install = mca_ptl_sm.la
else
module_noinst = libmca_ptl_sm.la
module_install =
endif

mcamoduledir = $(libdir)/openmpi
mcamodule_LTLIBRARIES = $(module_install)
mca_ptl_sm_la_SOURCES = 
mca_ptl_sm_la_LIBADD = \
        src/libmca_ptl_sm.la \
        $(LIBOMPI_LA)
mca_ptl_sm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_ptl_sm_la_SOURCES =
libmca_ptl_sm_la_LIBADD = src/libmca_ptl_sm.la
libmca_ptl_sm_la_LDFLAGS = -module -avoid-version

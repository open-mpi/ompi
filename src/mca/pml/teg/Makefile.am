#
# $HEADER$
#

# Use the top-level LAM Makefile.options

include $(top_lam_srcdir)/config/Makefile.options

SUBDIRS = src

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# liblam_ssi_coll_lam_basic.la

if LAM_BUILD_pml_teg_LOADABLE_MODULE
module_noinst =
module_install = mca_pml_teg.la
else
module_noinst = libmca_pml_teg.la
module_install =
endif

mcamoduledir = $(libdir)/lam
mcamodule_LTLIBRARIES = $(module_install)
mca_pml_teg_la_SOURCES = 
mca_pml_teg_la_LIBADD = \
        src/libmca_pml_teg.la \
        $(LIBLAM_LA)
mca_pml_teg_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_pml_teg_la_SOURCES =
libmca_pml_teg_la_LIBADD = src/libmca_pml_teg.la
libmca_pml_teg_la_LDFLAGS = -module -avoid-version

#
# $HEADER$
#

# Use the top-level LAM Makefile.options

include $(top_lam_srcdir)/config/Makefile.options

SUBDIRS = src
DIST_SUBDIRS = config $(SUBDIRS)

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# liblam_ssi_coll_lam_basic.la

if LAM_BUILD_LOADABLE_MODULE
module_noinst =
module_install = mca_pml_teg.la
else
module_noinst = libmca_mpi_pml_teg.la
module_install =
endif

lamssiexecdir = $(libdir)/lam
lamssiexec_LTLIBRARIES = $(module_install)
mca_pml_teg_la_SOURCES = $(sources)
mca_pml_teg_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_mpi_pml_teg_la_SOURCES = $(sources)
libmca_mpi_pml_teg_la_LDFLAGS = -module -avoid-version

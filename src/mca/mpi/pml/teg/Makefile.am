#
# $HEADER$
#

# Use the top-level LAM Makefile.options

include $(top_lam_srcdir)/config/Makefile.options

EXTRA_DIST = VERSION

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

# Source code files

sources = \
	comm.h \
	proc.h \
	ptl_array.h \
	teg.h \
	pml_teg_proc.c \
        pml_teg_ptl_array.c \
	pml_teg.c

# For static MCA modules, we have to make the output library here in
# the top-level directory, and it has to be named
# libmca_{library}_{type}_{name}.la.  For dynamic modules, we build
# mca_{type}_{name}.la and install them into $(libdir)/lam.

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


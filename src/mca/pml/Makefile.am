#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_pml_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_pml_ALL_SUBDIRS)

# Source code files

headers = pml.h

noinst_LTLIBRARIES = libmca_pml.la
libmca_pml_la_SOURCES =
libmca_pml_la_LIBADD = \
        base/libmca_pml_base.la \
        $(MCA_pml_STATIC_LTLIBS)
libmca_pml_la_DEPENDENCIES = $(libmca_pml_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/pml
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

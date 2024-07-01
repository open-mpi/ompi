# -*- makefile -*-
#
# Copyright (c) 2006-2019 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2021 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2022      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# Note that Automake's Fortran-buidling rules uses CPPFLAGS and
# AM_CPPFLAGS.  This can cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253).  Let's just zero
# those out and rely on AM_FCFLAGS.
CPPFLAGS =
AM_CPPFLAGS =

# This Makefile is only relevant if we're building the ignore-TKR "use
# mpi" MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPI_IGNORE_TKR_BINDINGS

AM_FCFLAGS = -I$(top_builddir)/ompi/include -I$(top_srcdir)/ompi/include \
             -I$(top_builddir)/ompi/mpi/fortran/use-mpi \
             -I$(top_builddir) -I$(top_srcdir) $(FCFLAGS_f90)

lib_LTLIBRARIES = lib@OMPI_LIBMPI_NAME@_usempi_ignore_tkr.la

mpi-ignore-tkr-interfaces.h: mpi-ignore-tkr-interfaces.h.in
pmpi-ignore-tkr-interfaces.h: mpi-ignore-tkr-interfaces.h
mpi-ignore-tkr-file-interfaces.h: mpi-ignore-tkr-file-interfaces.h.in
pmpi-ignore-tkr-file-interfaces.h: mpi-ignore-tkr-file-interfaces.h
mpi-ignore-tkr-removed-interfaces.h: mpi-ignore-tkr-removed-interfaces.h.in
pmpi-ignore-tkr-removed-interfaces.h: mpi-ignore-tkr-removed-interfaces.h

mpi-ignore-tkr.lo: $(top_srcdir)/ompi/mpi/fortran/base/attr-fn-int-callback-interfaces.h
mpi-ignore-tkr.lo: $(top_srcdir)/ompi/mpi/fortran/base/conversion-fn-null-int-interface.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-interfaces.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-status.h
mpi-ignore-tkr.lo: pmpi-ignore-tkr-interfaces.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-file-interfaces.h
mpi-ignore-tkr.lo: pmpi-ignore-tkr-file-interfaces.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-removed-interfaces.h
mpi-ignore-tkr.lo: pmpi-ignore-tkr-removed-interfaces.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-sizeof.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-sizeof.f90
mpi-ignore-tkr.lo: mpi-ignore-tkr.F90

lib@OMPI_LIBMPI_NAME@_usempi_ignore_tkr_la_SOURCES = \
        mpi-ignore-tkr.F90 \
        mpi-ignore-tkr-status.h \
        pmpi-ignore-tkr-interfaces.h \
        pmpi-ignore-tkr-file-interfaces.h \
        pmpi-ignore-tkr-removed-interfaces.h

nodist_lib@OMPI_LIBMPI_NAME@_usempi_ignore_tkr_la_SOURCES = \
        mpi-ignore-tkr-interfaces.h \
        mpi-ignore-tkr-file-interfaces.h \
        mpi-ignore-tkr-removed-interfaces.h

if BUILD_FORTRAN_SIZEOF
# These files are generated; do not distribute them
nodist_lib@OMPI_LIBMPI_NAME@_usempi_ignore_tkr_la_SOURCES += \
        mpi-ignore-tkr-sizeof.h \
        mpi-ignore-tkr-sizeof.f90
endif

libusempi_internal_modules =
if OMPI_FORTRAN_HAVE_TYPE_MPI_STATUS
libusempi_internal_modules += $(top_builddir)/ompi/mpi/fortran/use-mpi/libusempi_internal_modules.la
endif

# Note that we invoke some OPAL functions directly in
# libmpi_usempi_ignore_tkr.la, so we need to link in the OPAL library
# directly (pulling it in indirectly via libmpi.la does not work on
# all platforms).
lib@OMPI_LIBMPI_NAME@_usempi_ignore_tkr_la_LIBADD = \
        $(libusempi_internal_modules) \
        $(OMPI_TOP_BUILDDIR)/opal/lib@OPAL_LIB_NAME@.la
lib@OMPI_LIBMPI_NAME@_usempi_ignore_tkr_la_LDFLAGS = \
        -version-info $(libmpi_usempi_ignore_tkr_so_version) \
        $(OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS)

#
# mpi-ignore-tkr-sizeof.* are generated based on some results from
# configure tests.
#

sizeof_pl=$(top_srcdir)/ompi/mpi/fortran/base/gen-mpi-sizeof.pl

mpi-ignore-tkr-sizeof.h: $(top_builddir)/config.status
mpi-ignore-tkr-sizeof.h: $(sizeof_pl)
mpi-ignore-tkr-sizeof.h:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --header=$@ --ierror=mandatory \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF) \
	    --real2=$(OMPI_HAVE_FORTRAN_REAL2) \
	    --iso_real16=$(OMPI_FORTRAN_HAVE_ISO_FORTRAN_ENV_REAL16) \
	    --real16=$(OMPI_HAVE_FORTRAN_REAL16) \
	    --complex4=$(OMPI_HAVE_FORTRAN_COMPLEX4) \
	    --complex32=$(OMPI_HAVE_FORTRAN_COMPLEX32) \
	    --mpi_version=$(MPI_VERSION) \
	    --request_deprecate=$(OMPI_FORTRAN_HAVE_ATTR_DEPRECATED)

mpi-ignore-tkr-sizeof.f90: $(top_builddir)/config.status
mpi-ignore-tkr-sizeof.f90: $(sizeof_pl)
mpi-ignore-tkr-sizeof.f90:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --impl=$@ --ierror=mandatory --mpi --pmpi \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF) \
	    --real2=$(OMPI_HAVE_FORTRAN_REAL2) \
	    --iso_real16=$(OMPI_FORTRAN_HAVE_ISO_FORTRAN_ENV_REAL16) \
	    --real16=$(OMPI_HAVE_FORTRAN_REAL16) \
	    --complex4=$(OMPI_HAVE_FORTRAN_COMPLEX4) \
	    --complex32=$(OMPI_HAVE_FORTRAN_COMPLEX32)

#
# Clean up generated and module files
#

CLEANFILES += mpi-ignore-tkr-sizeof.h mpi-ignore-tkr-sizeof.f90
MOSTLYCLEANFILES = *.mod
CLEANFILES += *.i90

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR); \
	  $(INSTALL) $$file $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR)/$$file; \
	  rm -f $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR)/$$file; \
	done

endif

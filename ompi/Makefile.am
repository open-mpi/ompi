#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2010-2011 Sandia National Laboratories. All rights reserved.
# Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2015-2020 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# Copyright (c) 2025-2026 Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# See if we have Fortran mpif.h MPI bindings

if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS
mpi_fortran_base_lib = mpi/fortran/base/libmpi_fortran_base.la
else
mpi_fortran_base_lib =
endif

# Note that the ordering of "." in SUBDIRS is important: the Fortran
# mpif.h, and use mpi/use mpi_f08 bindings are all in standalone .la
# files that depend on libmpi.la.  So we must fully build libmpi.la
# first.

# NOTE: A handful of files in mpi/fortran/base must be included in
# libmpi.la.  But we wanted to keep all the Fortran sources together
# in the same tree, so we moved those sources to a separate
# subdirectory with its own Makefile.include that is included in this
# Makefile.am (NOTE: it did *not* work to put all the files -- base
# and non-base -- into mpi/fortran/mpif-h and have both a regular
# Makefile.am for building the mpif.h bindings library and a separate
# Makefile.include that was included in this top-level Makefile.am;
# problems occurred with "make distclean" and files in the
# ompi/mpi/fortran/mpif-h/.deps directory -- it's not clear whether
# this is an AM bug or whether this behavior is simply not supported).
# This ompi/mpi/fortran/base/Makefile.include file makes a convenience
# LT library that is then sucked into libmpi.la (the
# ompi/mpi/fortran/base sources must be compiled with special
# CPPFLAGS; we can't just add the raw sources to libmpi_la_SOURCES,
# unfortunately).

# The end of the result is that libmpi.la -- including a few sources
# from mpi/fortran/base -- is fully built before the mpif.h, and
# use mpi/use mpi_f08 bindings are built.  Therefore, the mpif.h
# and use mpi/use mpi_f08 bindings libraries can all link against
# libmpi.la.

# NOTE: The ordering of this list is VERY CAREFULLY CRAFTED!  Do not
# change the ordering unless you really know what you're doing.  :-)

SUBDIRS = \
        include \
        datatype \
        debuggers \
        etc \
        mpi/c \
        mpi/tool \
        $(OMPI_MPIEXT_C_DIRS) \
        mpi/fortran/base/ \
        $(MCA_ompi_FRAMEWORKS_SUBDIRS) \
        $(MCA_ompi_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
        $(OMPI_MPIEXT_MPIFH_DIRS) \
        mpi/fortran/mpif-h \
        mpi/fortran/use-mpi \
        $(OMPI_MPIEXT_USEMPI_DIR) \
        mpi/fortran/use-mpi-ignore-tkr \
        mpi/fortran/mpiext-use-mpi \
        mpi/fortran/use-mpi-f08/base \
        mpi/fortran/use-mpi-f08/mod \
        mpi/fortran/use-mpi-f08/bindings \
        $(OMPI_MPIEXT_USEMPIF08_DIRS) \
        mpi/fortran/use-mpi-f08 \
        mpi/fortran/mpiext-use-mpi-f08 \
        $(MCA_ompi_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

# Unit tests for the OMPI layer.  Placed at the end of SUBDIRS so that
# libmpi (built by the "." entry above) exists before the test programs
# are linked during "make check".
SUBDIRS += \
        test

# The ordering of the DIST_SUBDIRS isn't as important, but note that
# its contents *are* different than SUBDIRS.  In particular, the
# MPIEXT subdirs has a different value that is *not* equivalent to the
# union of the various $(OMPI_MPIEXT_*_DIRS) variables from SUBDIRS!

DIST_SUBDIRS = \
        include \
        datatype \
        debuggers \
        etc \
        mpi/c \
        mpi/tool \
        mpi/fortran/base \
        mpi/fortran/mpif-h \
        mpi/fortran/use-mpi \
        mpi/fortran/use-mpi-ignore-tkr \
        mpi/fortran/mpiext-use-mpi \
        mpi/fortran/use-mpi-f08 \
        mpi/fortran/use-mpi-f08/base \
        mpi/fortran/use-mpi-f08/mod \
        mpi/fortran/use-mpi-f08/bindings \
        mpi/fortran/mpiext-use-mpi-f08 \
        test \
        $(OMPI_MPIEXT_ALL_SUBDIRS) \
        $(MCA_ompi_FRAMEWORKS_SUBDIRS) \
        $(MCA_ompi_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

lib_LTLIBRARIES = libopen_mpi.la lib@OMPI_LIBMPI_NAME@.la
if OMPI_STANDARD_ABI
lib_LTLIBRARIES += libmpi_abi.la
endif

#
# ABI Refactor
#
# As required by the standard ABI, ompi must now provide libmpi_abi.la for
# ABI-specific code. Since we also want to avoid breaking the existing ompi
# ABI, the libraries in this directory are now organized as follows:
#
# +----------------------+----------------------+
# |     libmpi_abi.la    |        libmpi.la     |
# +----------------------+----------------------+
# |              libopen_mpi.la                 |
# +----------------------+----------------------+
#
# This includes a new library, libopen_mpi.la, that links in all backend code
# built in this directory or SUBDIRs of this directory. Previously everything
# was just linked directly into libmpi.la (lib@OMPI_LIBMPI_NAME@.la).
#
# libmpi_abi.la and libmpi.la both now link in libopen_mpi.la, the only
# difference between them being that one includes the standard ABI functions
# and the other the ompi-specific versions of those.
#

# Build the Open MPI internal library
libopen_mpi_la_SOURCES =
libopen_mpi_la_LIBADD = \
        datatype/libdatatype.la \
        debuggers/libdebuggers.la \
        $(OMPI_TOP_BUILDDIR)/opal/lib@OPAL_LIB_NAME@.la \
        $(MCA_ompi_FRAMEWORK_LIBS) \
        $(OMPI_LIBMPI_EXTRA_LIBS)
libopen_mpi_la_DEPENDENCIES = $(libopen_mpi_la_LIBADD)

# $(OMPI_NO_UNDEFINED_LDFLAGS) makes an unresolved symbol in libopen_mpi a
# hard link error.  libmpi is deliberately not on libopen_mpi's link line,
# so back-end code that calls a public MPI_* entry point fails to link
# here -- which is exactly what we want, because that dependency runs the
# wrong way.  See config/ompi_setup_layering_checks.m4.
libopen_mpi_la_LDFLAGS = \
       -version-info $(libopen_mpi_so_version) \
        $(OMPI_NO_UNDEFINED_LDFLAGS) \
        $(OMPI_LIBMPI_EXTRA_LDFLAGS)

# Build the main MPI library
#lib@OMPI_LIBMPI_NAME@_la_SOURCES = \
#        mpi/c/predefined_symbols_shim.c
lib@OMPI_LIBMPI_NAME@_la_SOURCES =

if OMPI_REEXPORT_MACOS
# On macOS, libopen_mpi is linked (and re-exported) via the
# -reexport-lopen_mpi flag below, NOT via LIBADD: if libopen_mpi.la
# also appeared in LIBADD, ld64 would record an ordinary LC_LOAD_DYLIB
# reference and drop the LC_REEXPORT_DYLIB attribute that provides
# v5.x binary compatibility.  But the flag is invisible to libtool, so
# libtool would not record libopen_mpi's own dependencies in
# libmpi.la; list libopen-pal.la explicitly so that uninstalled
# programs linking only libmpi.la still get opal/.libs in their
# runtime search path (otherwise "make check" before "make install"
# cannot load libopen-pal).
lib@OMPI_LIBMPI_NAME@_la_LIBADD = \
        mpi/c/libmpi_c.la \
        mpi/tool/libmpi_mpit.la \
        $(c_mpi_lib) \
        $(c_pmpi_lib) \
        $(OMPI_MPIEXT_C_LIBS) \
        $(mpi_fortran_base_lib) \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
# libopen_mpi.la is not in LIBADD (see above), but libmpi cannot link
# until it exists; keep it in DEPENDENCIES.
lib@OMPI_LIBMPI_NAME@_la_DEPENDENCIES = \
	libopen_mpi.la \
	$(lib@OMPI_LIBMPI_NAME@_la_LIBADD)
else
lib@OMPI_LIBMPI_NAME@_la_LIBADD = \
	libopen_mpi.la \
        mpi/c/libmpi_c.la \
        mpi/tool/libmpi_mpit.la \
        $(c_mpi_lib) \
        $(c_pmpi_lib) \
        $(OMPI_MPIEXT_C_LIBS) \
        $(mpi_fortran_base_lib)
lib@OMPI_LIBMPI_NAME@_la_DEPENDENCIES = $(lib@OMPI_LIBMPI_NAME@_la_LIBADD)
endif

lib@OMPI_LIBMPI_NAME@_la_LDFLAGS = \
       -version-info $(libmpi_so_version) \
        $(OMPI_LIBMPI_EXTRA_LDFLAGS)

# On macOS, use -reexport-l to re-export symbols from libopen_mpi;
# this also serves as the link dependency (removed from LIBADD above).
# ELF platforms need no re-export mechanism: the flat namespace
# resolves a v5.x binary's references from libopen_mpi via libmpi's
# DT_NEEDED dependency on it.
if OMPI_REEXPORT_MACOS
lib@OMPI_LIBMPI_NAME@_la_LDFLAGS += -L$(abs_builddir)/.libs -Wl,-reexport-lopen_mpi
endif

# The MPI Standard ABI library
libmpi_abi_la_SOURCES =
libmpi_abi_la_LIBADD = \
        libopen_mpi.la \
	mpi/c/libmpi_c_abi.la \
	mpi/tool/libmpi_mpit_abi.la
libmpi_abi_la_DEPENDENCIES = $(libmpi_abi_la_LIBADD)

libmpi_abi_la_LDFLAGS = \
       -version-info $(libmpi_abi_so_version) \
        $(OMPI_LIBMPI_EXTRA_LDFLAGS)

# included subdirectory Makefile.am's and appended-to variables
headers =
include_HEADERS =
EXTRA_DIST =
lib@OMPI_LIBMPI_NAME@_la_SOURCES += $(headers)
dist_ompidata_DATA =
libopen_mpi_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
nobase_ompi_HEADERS = $(headers)
endif

include class/Makefile.am
include attribute/Makefile.am
include communicator/Makefile.am
include dpm/Makefile.am
include errhandler/Makefile.am
include file/Makefile.am
include group/Makefile.am
include info/Makefile.am
include interlib/Makefile.am
include message/Makefile.am
include op/Makefile.am
include peruse/Makefile.am
include proc/Makefile.am
include request/Makefile.am
include runtime/Makefile.am
include win/Makefile.am
include tools/Makefile.am
include mpi/Makefile.am
include mpiext/Makefile.am
include mca/Makefile.am
include util/Makefile.am
include instance/Makefile.am

# Enforce Open MPI's library layering rule: libopen_mpi is the lower layer,
# and libmpi / libmpi_abi are the thin front-ends above it.  libopen_mpi
# must therefore never reference the public MPI_* / PMPI_* / MPIX_* /
# PMPIX_* entry points or the OMPI_* extensions, all of which are defined
# in the layer above.  Doing so creates a circular library dependency.
#
# $(OMPI_NO_UNDEFINED_LDFLAGS) already catches this at link time on
# platforms whose linker can reject undefined symbols in a shared library.
# This check is the authoritative one, though, because it also covers
# static (--disable-shared) builds -- which have no link step at all -- and
# because it still fires if someone "fixes" a link error by putting libmpi
# on libopen_mpi's link line, which is precisely the regression we are
# guarding against.
all-local: check-ompi-layering

.PHONY: check-ompi-layering
check-ompi-layering: libopen_mpi.la
	$(AM_V_at)$(SHELL) $(top_srcdir)/config/ompi_check_layering.sh "$(NM)" \
	    `echo .libs/libopen_mpi.a .libs/libopen_mpi*.so* .libs/libopen_mpi*.dylib`

distclean-local:
	rm -f mpiext/static-components.h

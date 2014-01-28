# -*- Mode: Makefile; -*-
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# (C) 2011 by Argonne National Laboratory.
#     See COPYRIGHT in top-level directory.
#

# OMPI: include a top level makefile with some options
include $(top_srcdir)/Makefile.options

## TODO: need to write an automakefile that handles two primary cases:
## 1) that ROMIO is being embedded within the MPI library, as in MPICH or Open
##    MPI
## 2) that ROMIO is being built standalone, old-school style.  This case is
##    basically unused in modern practice.

# help autoreconf and friends realize where the macros live
ACLOCAL_AMFLAGS = -I confdb

# empty variable initializations so that later code can append (+=)
include_HEADERS =
nodist_include_HEADERS =
noinst_HEADERS =
EXTRA_DIST =
SUFFIXES =
doc1_src_txt =

# ------------------------------------------------------------------------
# variables to be populated by the included Makefile.mk fragments:

# These are files that contain MPI routines (e.g., MPI_File_open).
# In MPICH these will have an MPI_ and a PMPI_ version.  Other implementations
# (like OMPI) only want these to be MPI_ routines, possibly with some
# name-shifting prefix.
romio_mpi_sources =

# regular old source files that implement ROMIO, such as ADIO code
romio_other_sources =

# code that may need to be "up" called from the MPI library and/or is
# MPI-implementation-specific in some way
glue_sources =

# ------------------------------------------------------------------------
# when building under MPICH we must be able to find mpi.h
AM_CPPFLAGS += $(MPI_H_INCLUDE)

# ------------------------------------------------------------------------
# handle the "include" directory here
AM_CPPFLAGS += -I$(top_builddir)/include -I$(top_srcdir)/include
# nodist_ b/c these are created by config.status and should not be distributed
# Open MPI: do not install mpio.h
noinst_HEADERS += include/mpio.h
noinst_HEADERS += include/io_romio_conv.h

# ------------------------------------------------------------------------

SUBDIRS =
DIST_SUBDIRS = test test-internal

# for the sake of parallel make and avoiding an excessive number of convenience
# libs, we use a subdir automake fragment strategy
include mpi-io/Makefile.mk
include adio/Makefile.mk

EXTRA_DIST += autogen.sh

if BUILD_ROMIO_EMBEDDED
# Build a libtool convenience library that the enclosing MPI implementation can
# use by adding it to the right _LIBADD variable.
noinst_LTLIBRARIES = libromio_dist.la
libromio_dist_la_SOURCES = $(romio_mpi_sources) $(romio_other_sources) $(glue_sources)

## NOTE: ROMIO's old build system builds a bunch of _foo.o objects that contain
## PMPI_ implementations as well as calls to only other PMPI routines.  In
## MPICH, these are the objects that need to go into libmpich, while the foo.o
## objects should go into libpmpich.  Furthermore, the -D option for ROMIO's
## source files is different and inverted (in the boolean sense) compared with
## MPICH's defintion.  And ROMIO was dumping all of the symbols into the main
## libmpich library, regardless of the separate profiling library's existence.
##
## Annoying, right?
if BUILD_PROFILING_LIB
# The current best strategy for now is to build the PMPI symbols as a separate
# convenience lib to permit adding the special "-D..." argument for all objects.
# MPICH will then link in both convenience library into libmpich, since it
# won't work very well the other way around.
noinst_LTLIBRARIES += libpromio.la
libpromio_la_SOURCES = $(romio_mpi_sources)
libpromio_la_CPPFLAGS = $(AM_CPPFLAGS) -DMPIO_BUILD_PROFILING
endif BUILD_PROFILING_LIB

else !BUILD_ROMIO_EMBEDDED
## TODO build a libromio.la (non-convenience) and possibly a libglue.la or something?

endif

# --------------------------------------------------------------------------
.PHONY: coverage
gcov_sources = $(libmpl_la_SOURCES)
# assumes that these sources were compiled appropriately ("-fprofile-arcs"
# and "-ftest-coverage")
coverage:
	@for file in $(gcov_sources) ; do \
	    dir=`dirname $$file` ; \
	    bname=`basename $$file` ; \
	    aux=`echo $$bname | sed -e 's,\.*$$,,'` ; \
	    echo "( $(GCOV) -b -f -o $$file $$file && mv $${bname}.gcov $$dir )" ; \
	    ( $(GCOV) -b -f -o $$file $$file && mv $${bname}.gcov $$dir ) ; \
	    rm -f *.gcov ; \
	done
	for subdir in $(SUBDIRS) - ; do \
	    if test $$subdir = "-" ; then break ; fi ; \
	    ( cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) coverage ) ; \
	done
# --------------------------------------------------------------------------
.PHONY: mandoc mandoc-local htmldoc htmldoc-local
SUFFIXES += .man-phony .html-phony .man1-phony .html1-phony .txt

# "make V=1" support for our documentation recipes
doctextman_verbose = $(doctextman_verbose_$(V))
doctextman_verbose_ = $(doctextman_verbose_$(AM_DEFAULT_VERBOSITY))
doctextman_verbose_0 = @echo "   DOCTEXTMAN  " $@;
doctexthtml_verbose = $(doctexthtml_verbose_$(V))
doctexthtml_verbose_ = $(doctexthtml_verbose_$(AM_DEFAULT_VERBOSITY))
doctexthtml_verbose_0 = @echo "   DOCTEXTHTML  " $@;

# Build dir paths where the man pages will be created.  Will usually be
# overridden by MPICH make.
mandoc_path1=$(abs_top_builddir)/man/man1
mandoc_path3=$(abs_top_builddir)/man/man3
htmldoc_path1=$(abs_top_builddir)/www/www1
htmldoc_path3=$(abs_top_builddir)/www/www3
doctext_docnotes=

.c.man-phony:
	$(doctextman_verbose)$(DOCTEXT) -man -mpath $(mandoc_path3) -ext 3 \
	    -heading MPI -quotefmt $(doctext_docnotes) $<
.c.html-phony:
	$(doctexthtml_verbose)$(DOCTEXT) -html -mpath $(htmldoc_path3) \
	    -heading MPI -quotefmt $(doctext_docnotes) $<

.txt.man1-phony:
	$(doctextman_verbose)$(DOCTEXT) -man -mpath $(mandoc_path1) -ext 1 \
	    -heading MPI -quotefmt $(doctext_docnotes) $<
.txt.html1-phony:
	$(doctexthtml_verbose)$(DOCTEXT) -html -mpath $(htmldoc_path1) \
	    -heading MPI -quotefmt $(doctext_docnotes) $<

# use mandoc-local target to force directory creation before running DOCTEXT
mandoc:
	test -d $(mandoc_path1) || $(MKDIR_P) $(mandoc_path1)
	test -d $(mandoc_path3) || $(MKDIR_P) $(mandoc_path3)
	$(MAKE) $(AM_MAKEFLAGS) mandoc-local
mandoc-local: $(romio_mpi_sources:.c=.man-phony) $(doc1_src_txt:.txt=.man1-phony)

# use htmldoc-local target to force directory creation before running DOCTEXT
htmldoc:
	test -d $(top_builddir)/www/www1 || $(MKDIR_P) $(top_builddir)/www/www1
	test -d $(top_builddir)/www/www3 || $(MKDIR_P) $(top_builddir)/www/www3
	$(MAKE) $(AM_MAKEFLAGS) htmldoc-local
htmldoc-local: $(romio_mpi_sources:.c=.html-phony) $(doc1_src_txt:.txt=.html1-phony)

# --------------------------------------------------------------------------


# -*- makefile -*-
#
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# This Makefile is only relevant if we're building the ignore-TKR "use
# mpi" MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPI_IGNORE_TKR_BINDINGS

AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

AM_FCFLAGS = -I$(top_builddir)/ompi/include -I$(top_srcdir)/ompi/include \
             -I$(top_builddir) -I$(top_srcdir) $(FCFLAGS_f90)

lib_LTLIBRARIES = libmpi_usempi_ignore_tkr.la

mpi-ignore-tkr-interfaces.h: mpi-ignore-tkr-interfaces.h.in
mpi-ignore-tkr-file-interfaces.h: mpi-ignore-tkr-file-interfaces.h.in

mpi-ignore-tkr.lo: $(top_srcdir)/ompi/mpi/fortran/base/attr-fn-int-callback-interfaces.h
mpi-ignore-tkr.lo: $(top_srcdir)/ompi/mpi/fortran/base/conversion-fn-null-int-interface.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-interfaces.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-file-interfaces.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-sizeof.h
mpi-ignore-tkr.lo: mpi-ignore-tkr-sizeof.f90
mpi-ignore-tkr.lo: mpi-ignore-tkr.F90

libmpi_usempi_ignore_tkr_la_SOURCES = \
        mpi-ignore-tkr.F90
nodist_libmpi_usempi_ignore_tkr_la_SOURCES = \
        mpi-ignore-tkr-interfaces.h \
        mpi-ignore-tkr-file-interfaces.h

if BUILD_FORTRAN_SIZEOF
# These files are generated; do not distribute them
nodist_libmpi_usempi_ignore_tkr_la_SOURCES += \
        mpi-ignore-tkr-sizeof.h \
        mpi-ignore-tkr-sizeof.f90
endif

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
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF)

mpi-ignore-tkr-sizeof.f90: $(top_builddir)/config.status
mpi-ignore-tkr-sizeof.f90: $(sizeof_pl)
mpi-ignore-tkr-sizeof.f90:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --impl=$@ --ierror=mandatory --mpi --pmpi \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF)

#
# Clean up generated and module files
#

CLEANFILES += mpi-ignore-tkr-sizeof.h mpi-ignore-tkr-sizeof.f90
MOSTLYCLEANFILES = *.mod

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(libdir); \
	  $(INSTALL) $$file $(DESTDIR)$(libdir); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(libdir)/$$file; \
	  rm -f $(DESTDIR)$(libdir)/$$file; \
	done
else

# Need to have empty targets because AM can't handle having an
# AM_CONDITIONAL was targets in the "if" statement but not in the
# "else".  :-(

install-exec-hook:
uninstall-local:

endif

#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# This is ugly, but we need it so that people can change things at
# "make" time (e.g., "make sysconfdir=/foo/bar all") as documented in
# autoconf.

AM_CPPFLAGS   = \
      -DOMPI_PREFIX="\"$(prefix)\"" \
      -DOMPI_INCDIR="\"$(includedir)\"" \
      -DOMPI_LIBDIR="\"$(libdir)\""

EXTRA_DIST = wrap.cc wrap_engine.cc $(pkgdata_DATA)

pkgdata_DATA = help-wrapper.txt

bin_PROGRAMS = mpicc mpic++ mpif77 mpif90

libs = $(top_builddir)/src/libmpi.la

wrap_sources = wrap.cc wrap_engine.cc ompi_wrap.h

mpicc_SOURCES       = mpicc.cc ompi_wrap.h $(wrap_sources)
mpicc_LDADD         = $(libs)  $(LIBOMPI_EXTRA_LIBS)
mpicc_LDFLAGS       = $(LIBOMPI_EXTRA_LDFLAGS)
mpicc_DEPENDENCIES  = $(libs)

mpic___SOURCES      = mpicxx.cc ompi_wrap.h $(wrap_sources)
mpic___LDADD        = $(libs) $(LIBOMPI_EXTRA_LIBS)
mpic___LDFLAGS      = $(LIBOMPI_EXTRA_LDFLAGS)
mpic___DEPENDENCIES = $(libs)

mpif77_SOURCES      = mpif77.cc ompi_wrap.h $(wrap_sources)
mpif77_LDADD        = $(libs) $(LIBOMPI_EXTRA_LIBS)
mpif77_LDFLAGS      = $(LIBOMPI_EXTRA_LDFLAGS)
mpif77_DEPENDENCIES = $(libs)

mpif90_SOURCES      = mpif90.cc ompi_wrap.h $(wrap_sources)
mpif90_LDADD        = $(libs) $(LIBOMPI_EXTRA_LIBS)
mpif90_LDFLAGS      = $(LIBOMPI_EXTRA_LDFLAGS)
mpif90_DEPENDENCIES = $(libs)

# Since this is C++ and we use templates (the STL), also ditch the
# template repository directory

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

if WANT_DEPRECATED_EXECUTABLE_NAMES
if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f hcc; ln -s mpicc hcc)
	(cd $(DESTDIR)$(bindir); rm -f hcp; ln -s mpiCC hcp)
	(cd $(DESTDIR)$(bindir); rm -f hf77; ln -s mpif77 hf77)
	(cd $(DESTDIR)$(bindir); rm -f mpiCC; ln -s mpic++ mpiCC)
else
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f hcc; ln -s mpicc hcc)
	(cd $(DESTDIR)$(bindir); rm -f hcp; ln -s mpic++ hcp)
	(cd $(DESTDIR)$(bindir); rm -f hf77; ln -s mpif77 hf77)
endif

#
# mpiCC might be a symlink we created, so be nice....
#
if CASE_SENSITIVE_FS
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hcc \
	$(DESTDIR)$(bindir)/hcp \
	$(DESTDIR)$(bindir)/hf77 \
	$(DESTDIR)$(bindir)/mpiCC
else
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hcc \
	$(DESTDIR)$(bindir)/hcp \
	$(DESTDIR)$(bindir)/hf77
endif

else
#
# If we don't have deprecated names, just do the mpiCC / mpic++ stuff.
# If we are on a non-case sensitive file system, don't do anything
# with mpiCC, since that is the same as mpicc, and there is no way
# that can end good.
#
if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpiCC; ln -s mpic++ mpiCC)
else
install-exec-hook:
endif

if CASE_SENSITIVE_FS
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpiCC
else
uninstall-local:
endif
endif

#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-wrapper.txt \
	ompicc-wrapper-data.txt \
	ompic++-wrapper-data.txt \
	ompif77-wrapper-data.txt \
	ompif90-wrapper-data.txt

# This is ugly, but we need it so that people can change things at
# "make" time (e.g., "make sysconfdir=/foo/bar all") as documented in
# autoconf.

AM_CPPFLAGS   = \
      -DOMPI_PREFIX="\"$(prefix)\"" \
      -DOMPI_INCDIR="\"$(includedir)\"" \
      -DOMPI_LIBDIR="\"$(libdir)\"" \
      -DOMPI_FC_MODULE_FLAG="\"$(OMPI_FC_MODULE_FLAG)\""

if WANT_INSTALL_HEADERS
AM_CPPFLAGS += -DOMPI_WANT_DEVEL_HEADERS=1
else
AM_CPPFLAGS += -DOMPI_WANT_DEVEL_HEADERS=0
endif

EXTRA_DIST = wrap.cc wrap_engine.cc

bin_PROGRAMS = mpicc mpic++ mpif77 mpif90

libs = \
        $(top_builddir)/opal/libopal.la

wrap_sources = wrap.cc wrap_engine.cc ompi_wrap.h

mpicc_SOURCES       = mpicc.cc ompi_wrap.h $(wrap_sources)
mpicc_LDADD         = $(libs)
mpicc_LDFLAGS       =
mpicc_DEPENDENCIES  = $(libs)

mpic___SOURCES      = mpicxx.cc ompi_wrap.h $(wrap_sources)
mpic___LDADD        = $(libs)
mpic___LDFLAGS      =
mpic___DEPENDENCIES = $(libs)

mpif77_SOURCES      = mpif77.cc ompi_wrap.h $(wrap_sources)
mpif77_LDADD        = $(libs)
mpif77_LDFLAGS      = 
mpif77_DEPENDENCIES = $(libs)

mpif90_SOURCES      = mpif90.cc ompi_wrap.h $(wrap_sources)
mpif90_LDADD        = $(libs)
mpif90_LDFLAGS      = 
mpif90_DEPENDENCIES = $(libs)

# Since this is C++ and we use templates (the STL), also ditch the
# template repository directory

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

if WANT_DEPRECATED_EXECUTABLE_NAMES
if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f hcc$(EXEEXT); $(LN_S) mpicc hcc)
	(cd $(DESTDIR)$(bindir); rm -f hcp$(EXEEXT); $(LN_S) mpiCC hcp)
	(cd $(DESTDIR)$(bindir); rm -f hf77$(EXEEXT); $(LN_S) mpif77 hf77)
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) mpic++ mpiCC)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) mpic++ mpicxx)
else
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f hcc$(EXEEXT); $(LN_S) mpicc hcc)
	(cd $(DESTDIR)$(bindir); rm -f hcp$(EXEEXT); $(LN_S) mpic++ hcp)
	(cd $(DESTDIR)$(bindir); rm -f hf77$(EXEEXT); $(LN_S) mpif77 hf77)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) mpic++ mpicxx)
endif

#
# mpiCC might be a symlink we created, so be nice....
#
if CASE_SENSITIVE_FS
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/hcp$(EXEEXT) \
	$(DESTDIR)$(bindir)/hf77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT)
else
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/hcp$(EXEEXT) \
	$(DESTDIR)$(bindir)/hf77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT)
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
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) mpic++ mpiCC)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) mpic++ mpicxx)

	(cd $(DESTDIR)$(bindir); rm -f ompicc$(EXEEXT); $(LN_S) opal_wrapper ompicc)
	(cd $(DESTDIR)$(bindir); rm -f ompic++$(EXEEXT); $(LN_S) opal_wrapper ompic++)
	(cd $(DESTDIR)$(bindir); rm -f ompiCC$(EXEEXT); $(LN_S) opal_wrapper ompiCC)
	(cd $(DESTDIR)$(bindir); rm -f ompicxx$(EXEEXT); $(LN_S) opal_wrapper ompicxx)
	(cd $(DESTDIR)$(bindir); rm -f ompif77$(EXEEXT); $(LN_S) opal_wrapper ompif77)
	(cd $(DESTDIR)$(bindir); rm -f ompif90$(EXEEXT); $(LN_S) opal_wrapper ompif90)

	(cd $(DESTDIR)$(pkgdatadir); rm -f ompiCC-wrapper-data.txt; $(LN_S) ompic++-wrapper-data.txt ompiCC-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f ompicxx-wrapper-data.txt; $(LN_S) ompic++-wrapper-data.txt ompicxx-wrapper-data.txt)
else
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) mpic++ mpicxx)

	(cd $(DESTDIR)$(bindir); rm -f ompicc$(EXEEXT); $(LN_S) opal_wrapper ompicc)
	(cd $(DESTDIR)$(bindir); rm -f ompic++$(EXEEXT); $(LN_S) opal_wrapper ompic++)
	(cd $(DESTDIR)$(bindir); rm -f ompicxx$(EXEEXT); $(LN_S) opal_wrapper ompicxx)
	(cd $(DESTDIR)$(bindir); rm -f ompif77$(EXEEXT); $(LN_S) opal_wrapper ompif77)
	(cd $(DESTDIR)$(bindir); rm -f ompif90$(EXEEXT); $(LN_S) opal_wrapper ompif90)

	(cd $(DESTDIR)$(pkgdatadir); rm -f ompicxx-wrapper-data.txt; $(LN_S) ompic++-wrapper-data.txt ompicxx-wrapper-data.txt)
endif

if CASE_SENSITIVE_FS
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompiCC$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompif90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/ompiCC-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/ompicxx-wrapper-data.txt

else
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/ompif90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/ompicxx-wrapper-data.txt
endif
endif

# -*- makefile -*-
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

libs = $(top_builddir)/src/libmpi.la

# This is ugly, but we need it so that people can change things at
# "make" time (e.g., "make sysconfdir=/foo/bar all") as documented in
# autoconf.

AM_CPPFLAGS   = \
      -DLAM_PREFIX="\"$(prefix)\"" \
      -DLAM_INCDIR="\"$(includedir)\"" \
      -DLAM_LIBDIR="\"$(libdir)\""

bin_PROGRAMS    = mpicc mpic++ mpif77 mpif90

# We unfortunately can't make a library for the common sources because
# making a library of C++ sources is not standardized.  :-( We also
# can't just list the same .cc files in multiple targets, because some
# C++ compilers will actually compile them multiple times (i.e., once
# for each target).  This results in a timestamp for wrap.o being
# later than already-finished targets (e.g., when wrap.o may end up
# with a timestamp before mpif77, but after mpicc).  So we have to sym
# link the common sources to unique names for each wrapper compiler
# and let them effectively be compiled once for each wrapper.

EXTRA_DIST    = wrap.cc wrap_engine.cc

mpicc_SOURCES       = mpicc.cc wrap_cc.cc wrap_engine_cc.cc
mpicc_LDADD         = $(libs) $(LIBLAM_EXTRA_LIBS)
mpicc_LDFLAGS       = $(LIBLAM_EXTRA_LDFLAGS)
mpicc_DEPENDENCIES  = $(libs)

mpic___SOURCES      = mpicxx.cc wrap_cxx.cc wrap_engine_cxx.cc
mpic___LDADD        = $(libs) $(LIBLAM_EXTRA_LIBS)
mpic___LDFLAGS      = $(LIBLAM_EXTRA_LDFLAGS)
mpic___DEPENDENCIES = $(libs)

mpif77_SOURCES      = mpif77.cc wrap_f77.cc wrap_engine_f77.cc
mpif77_LDADD        = $(libs) $(LIBLAM_EXTRA_LIBS)
mpif77_LDFLAGS      = $(LIBLAM_EXTRA_LDFLAGS)
mpif77_DEPENDENCIES = $(libs)

mpif90_SOURCES      = mpif90.cc wrap_f90.cc wrap_engine_f90.cc
mpif90_LDADD        = $(libs) $(LIBLAM_EXTRA_LIBS)
mpif90_LDFLAGS      = $(LIBLAM_EXTRA_LDFLAGS)
mpif90_DEPENDENCIES = $(libs)

# Make the sym links

wrap_cc.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap.cc wrap_cc.cc
wrap_engine_cc.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap_engine.cc wrap_engine_cc.cc
wrap_cxx.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap.cc wrap_cxx.cc
wrap_engine_cxx.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap_engine.cc wrap_engine_cxx.cc
wrap_f77.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap.cc wrap_f77.cc
wrap_engine_f77.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap_engine.cc wrap_engine_f77.cc
wrap_f90.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap.cc wrap_f90.cc
wrap_engine_f90.cc:
	ln -s $(top_srcdir)/src/tools/wrappers/wrap_engine.cc wrap_engine_f90.cc

# Since this is C++ and we use templates (the STL), also ditch the
# template repository directory

clean-local:
	test -z "$(LAM_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(LAM_CXX_TEMPLATE_REPOSITORY)
	rm -f wrap_cc.cc wrap_engine_cc.cc
	rm -f wrap_cxx.cc wrap_engine_cxx.cc
	rm -f wrap_f77.cc wrap_engine_f77.cc
	rm -f wrap_f90.cc wrap_engine_f90.cc


# Per bug 476, the "wipe" command is now "lamwipe".  So that we don't
# break backwards compatability, we provide sym link names to the old
# names (i.e., lamwipe -> wipe).  However, we provide a
# --disable-deprecated-executable-names option that will disable this
# behavior.  Someday, we'll change the default and the option will
# become --enable-deprecated-executable-names.

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
#
# Adding this to make sure that make install does not fail. 
# install-exec-hook needs to be defined or else make install
# fails quoting that there is not target called
# install-exec-hook if WANT_DEPRECATED_EXECUTABLE_NAMES is
# false.
else
install-exec-hook:
uninstall-local:
endif

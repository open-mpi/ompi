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
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013      Intel, Inc. All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

generated_man_pages = mpicc.1 mpic++.1 mpicxx.1 mpifort.1 mpif77.1 mpif90.1
man_pages = $(generated_man_pages)

EXTRA_DIST = mpif77.1in mpijavac.1 mpijavac.pl.in

if OMPI_WANT_JAVA_BINDINGS
man_pages += mpijavac.1
endif

if OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

bin_SCRIPTS = ompi_wrapper_script
if OMPI_WANT_JAVA_BINDINGS
bin_SCRIPTS += mpijavac.pl
endif
CLEANFILES += $(bin_SCRIPTS)

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mpicc; $(LN_S) ompi_wrapper_script mpicc)
	(cd $(DESTDIR)$(bindir); rm -f mpic++; $(LN_S) ompi_wrapper_script mpic++)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx; $(LN_S) ompi_wrapper_script mpicxx)
	(cd $(DESTDIR)$(bindir); rm -f mpifort; $(LN_S) ompi_wrapper_script mpifort)
	(cd $(DESTDIR)$(bindir); rm -f mpif77; $(LN_S) mpifort mpif77)
	(cd $(DESTDIR)$(bindir); rm -f mpif90; $(LN_S) mpifort mpif90)
if OMPI_WANT_JAVA_BINDINGS
	(cp mpijavac.pl $(DESTDIR)$(bindir))
	(cd $(DESTDIR)$(bindir); chmod +x mpijavac.pl; rm -f mpijavac; $(LN_S) mpijavac.pl mpijavac)
endif

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/mpicc \
	$(DESTDIR)$(bindir)/mpic++ \
	$(DESTDIR)$(bindir)/mpicxx \
	$(DESTDIR)$(bindir)/mpifort \
	$(DESTDIR)$(bindir)/mpif77 \
	$(DESTDIR)$(bindir)/mpif90 \
	$(DESTDIR)$(bindir)/mpijavac

if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f mpiCC; $(LN_S) ompi_wrapper_script mpiCC)
uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/mpiCC
else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
uninstall-local: uninstall-local-always
endif # CASE_SENSITIVE_FS

else # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

if OMPI_INSTALL_BINARIES

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ompi.pc ompi-c.pc ompi-cxx.pc ompi-fort.pc

if OMPI_WANT_JAVA_BINDINGS
bin_SCRIPTS = mpijavac.pl
endif

man_MANS = $(man_pages)

nodist_ompidata_DATA = \
        mpicc-wrapper-data.txt \
	mpic++-wrapper-data.txt \
	mpifort-wrapper-data.txt

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mpicc$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) mpicc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpic++$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) mpic++$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) mpicxx$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpifort$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) mpifort$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpif77$(EXEEXT); $(LN_S) mpifort$(EXEEXT) mpif77$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpif90$(EXEEXT); $(LN_S) mpifort$(EXEEXT) mpif90$(EXEEXT))
if OMPI_WANT_JAVA_BINDINGS
	(cp mpijavac.pl $(DESTDIR)$(bindir))
	(cd $(DESTDIR)$(bindir); chmod +x mpijavac.pl; rm -f mpijavac; $(LN_S) mpijavac.pl mpijavac)
endif

install-data-hook-always:
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpicxx-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpicxx-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpif77-wrapper-data.txt; $(LN_S) mpifort-wrapper-data.txt mpif77-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpif90-wrapper-data.txt; $(LN_S) mpifort-wrapper-data.txt mpif90-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgconfigdir); rm -f ompi-f77.pc; $(LN_S) ompi-fort.pc ompi-f77.pc)
	(cd $(DESTDIR)$(pkgconfigdir); rm -f ompi-f90.pc; $(LN_S) ompi-fort.pc ompi-f90.pc)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/mpicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif90$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpifort$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpicxx-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/mpif77-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/mpif90-wrapper-data.txt \
	$(DESTDIR)$(pkgconfigdir)/ompi-f77.pc \
	$(DESTDIR)$(pkgconfigdir)/ompi-f90.pc \
	$(DESTDIR)$(pkgconfigdir)/ompi-fort.pc \
	$(DESTDIR)$(bindir)/mpijavac \
	$(DESTDIR)$(bindir)/mpijavac.pl

if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) mpiCC$(EXEEXT))

install-data-hook: install-data-hook-always
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpiCC-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpiCC-wrapper-data.txt)
	(cd $(DESTDIR)$(mandir)/man1; rm -f mpiCC.1; $(LN_S) mpic++.1 mpiCC.1)

uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/mpiCC.1 \
	$(DESTDIR)$(pkgdatadir)/mpiCC-wrapper-data.txt

else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
install-data-hook: install-data-hook-always
uninstall-local: uninstall-local-always

endif # CASE_SENSITIVE_FS

endif # OMPI_INSTALL_BINARIES

endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS


########################################################
#
# Man page generation / handling
#
########################################################
distclean-local:
	rm -f $(generated_man_pages)

$(top_builddir)/opal/tools/wrappers/generic_wrapper.1:
	(cd $(top_builddir)/opal/tools/wrappers && $(MAKE) $(AM_MAKEFLAGS) generic_wrapper.1)

mpicc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpicc.1
	sed -e 's/#COMMAND#/mpicc/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpicc.1

mpic++.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpic++.1
	sed -e 's/#COMMAND#/mpic++/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpic++.1

mpicxx.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpicxx.1
	sed -e 's/#COMMAND#/mpicxx/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpicxx.1

mpifort.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpifort.1
	sed -e 's/#COMMAND#/mpifort/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/Fortran/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpifort.1

# Deprecated
mpif77.1: mpif77.1in
	rm -f mpif77.1
	sed -e 's/#PROJECT#/Open MPI/g' \
	    -e 's/#PROJECT_SHORT#/OMPI/g' \
	    -e 's/#LANGUAGE#/Fortran/g' \
	    -e 's/#PACKAGE_NAME#/@PACKAGE_NAME@/g' \
	    -e 's/#PACKAGE_VERSION#/@PACKAGE_VERSION@/g' \
	    -e 's/#OMPI_DATE#/@OMPI_RELEASE_DATE@/g' \
	    < $(top_srcdir)/ompi/tools/wrappers/mpif77.1in > mpif77.1

# Deprecated
mpif90.1: mpif77.1
	rm -f mpif90.1
	ln -s mpif77.1 mpif90.1

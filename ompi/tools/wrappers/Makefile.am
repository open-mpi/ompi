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
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = \
	mpicc.1 \
	mpic++.1 \
	mpicxx.1 \
	mpif77.1 \
	mpif90.1

dist_pkgdata_DATA = mpicc-wrapper-data.txt \
	mpic++-wrapper-data.txt \
	mpif77-wrapper-data.txt \
	mpif90-wrapper-data.txt

if OMPI_INSTALL_BINARIES

man_MANS = \
	mpicc.1 \
	mpic++.1 \
	mpicxx.1 \
	mpif77.1 \
	mpif90.1

install-exec-hook-always:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mpicc$(EXEEXT); $(LN_S) opal_wrapper mpicc)
	(cd $(DESTDIR)$(bindir); rm -f mpic++$(EXEEXT); $(LN_S) opal_wrapper mpic++)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) opal_wrapper mpicxx)
	(cd $(DESTDIR)$(bindir); rm -f mpif77$(EXEEXT); $(LN_S) opal_wrapper mpif77)
	(cd $(DESTDIR)$(bindir); rm -f mpif90$(EXEEXT); $(LN_S) opal_wrapper mpif90)

install-data-hook-always:
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpicxx-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpicxx-wrapper-data.txt)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/mpicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpicxx-wrapper-data.txt

if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) opal_wrapper mpiCC)

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

maintainer-clean-local:
	rm -f mpicc.1 mpic++.1 mpicxx.1 mpif77.1 mpif90.1

mpicc.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f mpicc.1
	sed -e 's/@COMMAND@/mpicc/g' -e 's/@PROJECT@/Open MPI/g' -e 's/@PROJECT_SHORT@/OMPI/g' -e 's/@LANGUAGE@/C/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > mpicc.1

mpic++.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f mpic++.1
	sed -e 's/@COMMAND@/mpic++/g' -e 's/@PROJECT@/Open MPI/g' -e 's/@PROJECT_SHORT@/OMPI/g' -e 's/@LANGUAGE@/C++/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > mpic++.1

mpicxx.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f mpicxx.1
	sed -e 's/@COMMAND@/mpicxx/g' -e 's/@PROJECT@/Open MPI/g' -e 's/@PROJECT_SHORT@/OMPI/g' -e 's/@LANGUAGE@/C++/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > mpicxx.1

mpif77.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f mpif77.1
	sed -e 's/@COMMAND@/mpif77/g' -e 's/@PROJECT@/Open MPI/g' -e 's/@PROJECT_SHORT@/OMPI/g' -e 's/@LANGUAGE@/Fortran 77/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > mpif77.1

mpif90.1: $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1
	rm -f mpif90.1
	sed -e 's/@COMMAND@/mpif90/g' -e 's/@PROJECT@/Open MPI/g' -e 's/@PROJECT_SHORT@/OMPI/g' -e 's/@LANGUAGE@/Fortran 90/g' < $(top_srcdir)/opal/tools/wrappers/opal_wrapper.1 > mpif90.1

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

dist_pkgdata_DATA = mpicc-wrapper-data.txt \
	mpic++-wrapper-data.txt \
	mpif77-wrapper-data.txt \
	mpif90-wrapper-data.txt

if CASE_SENSITIVE_FS
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) mpic++ mpiCC)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) mpic++ mpicxx)

	(cd $(DESTDIR)$(bindir); rm -f mpicc$(EXEEXT); $(LN_S) opal_wrapper mpicc)
	(cd $(DESTDIR)$(bindir); rm -f mpic++$(EXEEXT); $(LN_S) opal_wrapper mpic++)
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) opal_wrapper mpiCC)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) opal_wrapper mpicxx)
	(cd $(DESTDIR)$(bindir); rm -f mpif77$(EXEEXT); $(LN_S) opal_wrapper mpif77)
	(cd $(DESTDIR)$(bindir); rm -f mpif90$(EXEEXT); $(LN_S) opal_wrapper mpif90)

	(cd $(DESTDIR)$(pkgdatadir); rm -f mpiCC-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpiCC-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpicxx-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpicxx-wrapper-data.txt)
else
install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) mpic++ mpicxx)

	(cd $(DESTDIR)$(bindir); rm -f mpicc$(EXEEXT); $(LN_S) opal_wrapper mpicc)
	(cd $(DESTDIR)$(bindir); rm -f mpic++$(EXEEXT); $(LN_S) opal_wrapper mpic++)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) opal_wrapper mpicxx)
	(cd $(DESTDIR)$(bindir); rm -f mpif77$(EXEEXT); $(LN_S) opal_wrapper mpif77)
	(cd $(DESTDIR)$(bindir); rm -f mpif90$(EXEEXT); $(LN_S) opal_wrapper mpif90)

	(cd $(DESTDIR)$(pkgdatadir); rm -f mpicxx-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpicxx-wrapper-data.txt)
endif

if CASE_SENSITIVE_FS
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpiCC-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/mpicxx-wrapper-data.txt

else
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpicxx-wrapper-data.txt
endif

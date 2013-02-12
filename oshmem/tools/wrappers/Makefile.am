# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

#this file is included in oshmem/Makefile.am


.PHONY: all


dist_pkgdata_DATA =  \
	shmemcc-wrapper-data.txt \
	shmemf77-wrapper-data.txt \
	shmemf90-wrapper-data.txt

install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f shmemrun$(EXEEXT); $(LN_S) mpirun shmemrun)
	(cd $(DESTDIR)$(bindir); rm -f shmemcc$(EXEEXT); $(LN_S) mpicc shmemcc)
	(cd $(DESTDIR)$(bindir); rm -f oshcc$(EXEEXT); $(LN_S) mpicc oshcc)
	(cd $(DESTDIR)$(bindir); rm -f shmem_info$(EXEEXT); $(LN_S) ompi_info shmem_info)
	(cd $(DESTDIR)$(bindir); rm -f shmemf77$(EXEEXT); $(LN_S) mpif77 shmemf77)
	(cd $(DESTDIR)$(bindir); rm -f shmemf90$(EXEEXT); $(LN_S) mpif90 shmemf90)

install-data-hook:
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshcc-wrapper-data.txt; $(LN_S) shmemcc-wrapper-data.txt oshcc-wrapper-data.txt)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/shmemcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmem_info$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemf77$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemf90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/shmemcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/shmemf77-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/shmemf90-wrapper-data.txt

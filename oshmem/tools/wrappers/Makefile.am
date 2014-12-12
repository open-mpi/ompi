# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

nodist_oshmemdata_DATA =  \
	shmemcc-wrapper-data.txt \
	shmemfort-wrapper-data.txt

if PROJECT_OSHMEM
# Only install / uninstall if we're building oshmem
install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f shmemrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) shmemrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) oshrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemcc$(EXEEXT); $(LN_S) mpicc$(EXEEXT) shmemcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshcc$(EXEEXT); $(LN_S) mpicc$(EXEEXT) oshcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemfort$(EXEEXT); $(LN_S) mpifort$(EXEEXT) shmemfort$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshfort$(EXEEXT); $(LN_S) mpifort$(EXEEXT) oshfort$(EXEEXT))
if OSHMEM_WANT_JAVA_BINDINGS
	(cd $(DESTDIR)$(bindir); rm -f shmemjavac$(EXEEXT); $(LN_S) mpijavac$(EXEEXT) shmemjavac$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshjavac$(EXEEXT); $(LN_S) mpijavac$(EXEEXT) oshjavac$(EXEEXT))
endif

install-data-hook:
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshcc-wrapper-data.txt; $(LN_S) shmemcc-wrapper-data.txt oshcc-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshfort-wrapper-data.txt; $(LN_S) shmemfort-wrapper-data.txt oshfort-wrapper-data.txt)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/shmemrun$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshrun$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemfort$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshfort$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemjavac$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshjavac$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/shmemcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/shmemfort-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshfort-wrapper-data.txt \
	$(DESTDIR)$(bindir)/oshjavac \
	$(DESTDIR)$(bindir)/shmemjavac

endif # PROJECT_OSHMEM

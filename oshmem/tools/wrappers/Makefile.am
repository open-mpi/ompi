# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

include $(top_srcdir)/Makefile.ompi-rules

if PROJECT_OSHMEM

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = oshmem.pc oshmem-c.pc oshmem-cxx.pc oshmem-fort.pc

nodist_oshmemdata_DATA =  \
	shmemcc-wrapper-data.txt \
	shmemc++-wrapper-data.txt \
	shmemfort-wrapper-data.txt

targets_install_exec = install-exec-always
targets_install_data = install-data-always
targets_uninstall_local = uninstall-local-always

mk_bindir:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"

mk_pkgdatadir:
	test -z "$(pkgdatadir)" || $(mkdir_p) "$(DESTDIR)$(pkgdatadir)"

install-exec-always: mk_bindir
	(cd $(DESTDIR)$(bindir); rm -f shmemcc$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshcc$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemc++$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemc++$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemcxx$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemcxx$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshc++$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshc++$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshcxx$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshcxx$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemfort$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemfort$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshfort$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshfort$(EXEEXT))

install-exec-rte: mk_bindir
	(cd $(DESTDIR)$(bindir); rm -f shmemrun$(EXEEXT); $(LN_S) prun$(EXEEXT) shmemrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshrun$(EXEEXT); $(LN_S) prun$(EXEEXT) oshrun$(EXEEXT))

install-data-always: mk_pkgdatadir
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshcc-wrapper-data.txt; $(LN_S) shmemcc-wrapper-data.txt oshcc-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f shmemcxx-wrapper-data.txt; $(LN_S) shmemc++-wrapper-data.txt shmemcxx-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshc++-wrapper-data.txt; $(LN_S) shmemc++-wrapper-data.txt oshc++-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshcxx-wrapper-data.txt; $(LN_S) shmemc++-wrapper-data.txt oshcxx-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshfort-wrapper-data.txt; $(LN_S) shmemfort-wrapper-data.txt oshfort-wrapper-data.txt)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/shmemcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemcxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshcxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemfort$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshfort$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/shmemcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/shmemcxx-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshcxx-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/shmemfort-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshfort-wrapper-data.txt

uninstall-local-rte:
	rm -f $(DESTDIR)$(bindir)/shmemrun$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshrun$(EXEEXT)

install-exec-cs: mk_bindir
	(cd $(DESTDIR)$(bindir); rm -f shmemCC$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemCC$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshCC$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshCC$(EXEEXT))

install-data-cs: mk_pkgdatadir
	(cd $(DESTDIR)$(pkgdatadir); rm -f shmemCC-wrapper-data.txt; $(LN_S) shmemcxx-wrapper-data.txt shmemCC-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshCC-wrapper-data.txt; $(LN_S) oshcxx-wrapper-data.txt oshCC-wrapper-data.txt)

uninstall-local-cs:
	rm -f $(DESTDIR)$(bindir)/shmemCC$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/shmemCC-wrapper-data.txt
	rm -f $(DESTDIR)$(bindir)/oshCC$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/oshCC-wrapper-data.txt

if CASE_SENSITIVE_FS
targets_install_exec += install-exec-cs

targets_install_data += install-data-cs

targets_uninstall_local += uninstall-local-cs
endif # CASE_SENSITIVE_FS

install-exec-hook: $(targets_install_exec)
install-data-hook: $(targets_install_data)
uninstall-local: $(targets_uninstall_local)

endif # PROJECT_OSHMEM

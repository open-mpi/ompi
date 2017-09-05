# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

include $(top_srcdir)/Makefile.ompi-rules

man_pages = oshcc.1 shmemcc.1 oshc++.1 shmemc++.1 oshcxx.1 shmemcxx.1 oshfort.1 shmemfort.1 oshrun.1 shmemrun.1

if PROJECT_OSHMEM
man_MANS = $(man_pages)

nodist_oshmemdata_DATA =  \
	shmemcc-wrapper-data.txt \
	shmemc++-wrapper-data.txt \
	shmemfort-wrapper-data.txt

# Only install / uninstall if we're building oshmem
install-exec-hook-always:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f shmemrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) shmemrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) oshrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemcc$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshcc$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemc++$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemc++$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemcxx$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemcxx$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshc++$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshc++$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshcxx$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshcxx$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemfort$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemfort$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshfort$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshfort$(EXEEXT))

install-data-hook-always:
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshcc-wrapper-data.txt; $(LN_S) shmemcc-wrapper-data.txt oshcc-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f shmemcxx-wrapper-data.txt; $(LN_S) shmemc++-wrapper-data.txt shmemcxx-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshc++-wrapper-data.txt; $(LN_S) shmemc++-wrapper-data.txt oshc++-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshcxx-wrapper-data.txt; $(LN_S) shmemc++-wrapper-data.txt oshcxx-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshfort-wrapper-data.txt; $(LN_S) shmemfort-wrapper-data.txt oshfort-wrapper-data.txt)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/shmemrun$(EXEEXT) \
	$(DESTDIR)$(bindir)/oshrun$(EXEEXT) \
	$(DESTDIR)$(bindir)/shmemcc$(EXEEXT) \
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


if CASE_SENSITIVE_FS
man_MANS += oshCC.1 shmemCC.1

install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f shmemCC$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) shmemCC$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshCC$(EXEEXT); $(LN_S) opal_wrapper$(EXEEXT) oshCC$(EXEEXT))

install-data-hook: install-data-hook-always
	(cd $(DESTDIR)$(pkgdatadir); rm -f shmemCC-wrapper-data.txt; $(LN_S) shmemcxx-wrapper-data.txt shmemCC-wrapper-data.txt)
	(cd $(DESTDIR)$(pkgdatadir); rm -f oshCC-wrapper-data.txt; $(LN_S) oshcxx-wrapper-data.txt oshCC-wrapper-data.txt)

uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/shmemCC$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/shmemCC.1 \
	$(DESTDIR)$(pkgdatadir)/shmemCC-wrapper-data.txt
	rm -f $(DESTDIR)$(bindir)/oshCC$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/oshCC.1 \
	$(DESTDIR)$(pkgdatadir)/oshCC-wrapper-data.txt

oshCC.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f oshCC.1
	sed -e 's/#COMMAND#/oshCC/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > oshCC.1

shmemCC.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f shmemCC.1
	sed -e 's/#COMMAND#/shmemCC/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > shmemCC.1

else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
install-data-hook: install-data-hook-always
uninstall-local: uninstall-local-always

endif # CASE_SENSITIVE_FS

########################################################
#
# Man page generation / handling
#
########################################################
distclean-local:
	rm -f $(man_MANS)

$(top_builddir)/opal/tools/wrappers/generic_wrapper.1:
	(cd $(top_builddir)/opal/tools/wrappers && $(MAKE) $(AM_MAKEFLAGS) generic_wrapper.1)

oshcc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f oshcc.1
	sed -e 's/#COMMAND#/oshcc/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > oshcc.1

shmemcc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f shmemcc.1
	sed -e 's/#COMMAND#/shmemcc/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > shmemcc.1

oshc++.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f oshc++.1
	sed -e 's/#COMMAND#/oshc++/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > oshc++.1

shmemc++.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f shmemc++.1
	sed -e 's/#COMMAND#/shmemc++/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > shmemc++.1

oshcxx.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f oshcxx.1
	sed -e 's/#COMMAND#/oshcxx/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > oshcxx.1

shmemcxx.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f shmemcxx.1
	sed -e 's/#COMMAND#/shmemcxx/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > shmemcxx.1

oshfort.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f oshfort.1
	sed -e 's/#COMMAND#/oshfort/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/Fortran/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > oshfort.1

shmemfort.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f shmemfort.1
	sed -e 's/#COMMAND#/shmemfort/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/Fortran/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > shmemfort.1

$(top_builddir)/orte/tools/orterun/orterun.1:
	(cd $(top_builddir)/orte/tools/orterun && $(MAKE) $(AM_MAKEFLAGS) orterun.1)

oshrun.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 oshrun.1

shmemrun.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 shmemrun.1

endif # PROJECT_OSHMEM

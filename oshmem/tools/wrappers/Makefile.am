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

include $(top_srcdir)/Makefile.ompi-rules

man_pages = oshcc.1 shmemcc.1 oshfort.1 shmemfort.1 oshrun.1 shmemrun.1

if PROJECT_OSHMEM
man_MANS = $(man_pages)

nodist_oshmemdata_DATA =  \
	shmemcc-wrapper-data.txt \
	shmemfort-wrapper-data.txt

# Only install / uninstall if we're building oshmem
install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f shmemrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) shmemrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) oshrun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemcc$(EXEEXT); $(LN_S) mpicc$(EXEEXT) shmemcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshcc$(EXEEXT); $(LN_S) mpicc$(EXEEXT) oshcc$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f shmemfort$(EXEEXT); $(LN_S) mpifort$(EXEEXT) shmemfort$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshfort$(EXEEXT); $(LN_S) mpifort$(EXEEXT) oshfort$(EXEEXT))

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
	$(DESTDIR)$(pkgdatadir)/shmemcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshcc-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/shmemfort-wrapper-data.txt \
	$(DESTDIR)$(pkgdatadir)/oshfort-wrapper-data.txt


########################################################
#
# Man page generation / handling
#
########################################################
distclean-local:
	rm -f $(man_pages)

$(top_builddir)/opal/tools/wrappers/generic_wrapper.1:
	(cd $(top_builddir)/opal/tools/wrappers && $(MAKE) $(AM_MAKEFLAGS) generic_wrapper.1)

oshcc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f oshcc.1
	sed -e 's/#COMMAND#/oshcc/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > oshcc.1

shmemcc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f shmemcc.1
	sed -e 's/#COMMAND#/shmemcc/g' -e 's/#PROJECT#/Open SHMEM/g' -e 's/#PROJECT_SHORT#/OSHMEM/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > shmemcc.1

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

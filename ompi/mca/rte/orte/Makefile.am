#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers = rte_orte.h

sources = \
        rte_orte_component.c \
        rte_orte_module.c

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
nobase_ompi_HEADERS = $(headers)
endif

# We only ever build this component statically
noinst_LTLIBRARIES = libmca_rte_orte.la
libmca_rte_orte_la_SOURCES =$(sources) $(headers)
libmca_rte_orte_la_LDFLAGS = -module -avoid-version
libmca_rte_orte_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

man_pages = mpirun.1 mpiexec.1 ompi-ps.1 ompi-clean.1 ompi-top.1 ompi-server.1 ompi-dvm.1 ompi-submit.1

if WANT_FT
man_pages += ompi-checkpoint.1 ompi-restart.1
endif

if OPAL_INSTALL_BINARIES
nodist_man_MANS = $(man_pages)

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpirun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpiexec$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpiexec$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-ps$(EXEEXT); $(LN_S) orte-ps$(EXEEXT) ompi-ps$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-clean$(EXEEXT); $(LN_S) orte-clean$(EXEEXT) ompi-clean$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-top$(EXEEXT); $(LN_S) orte-top$(EXEEXT) ompi-top$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-server$(EXEEXT); $(LN_S) orte-server$(EXEEXT) ompi-server$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-dvm$(EXEEXT); $(LN_S) orte-dvm$(EXEEXT) ompi-dvm$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-submit$(EXEEXT); $(LN_S) orte-submit$(EXEEXT) ompi-submit$(EXEEXT))
if WANT_FT
	(cd $(DESTDIR)$(bindir); rm -f ompi-checkpoint$(EXEEXT); $(LN_S) orte-checkpoint$(EXEEXT) ompi-checkpoint$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-restart$(EXEEXT); $(LN_S) orte-restart$(EXEEXT) ompi-restart$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-migrate$(EXEEXT); $(LN_S) orte-migrate$(EXEEXT) ompi-migrate$(EXEEXT))
endif

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun$(EXEEXT) \
		$(DESTDIR)$(bindir)/mpiexec$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-ps$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-clean$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-top$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-server$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-dvm$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-submit$(EXEEXT)
if WANT_FT
	rm -f $(DESTDIR)$(bindir)/ompi-checkpoint$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-restart$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-migrate$(EXEEXT)
endif

endif # OPAL_INSTALL_BINARIES

$(top_builddir)/orte/tools/orterun/orterun.1:
	(cd $(top_builddir)/orte/tools/orterun && $(MAKE) $(AM_MAKEFLAGS) orterun.1)

mpirun.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 mpirun.1

mpiexec.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 mpiexec.1

$(top_builddir)/orte/tools/orte-ps/orte-ps.1:
	(cd $(top_builddir)/orte/tools/orte-ps && $(MAKE) $(AM_MAKEFLAGS) orte-ps.1)

ompi-ps.1: $(top_builddir)/orte/tools/orte-ps/orte-ps.1
	cp -f $(top_builddir)/orte/tools/orte-ps/orte-ps.1 ompi-ps.1

$(top_builddir)/orte/tools/orte-clean/orte-clean.1:
	(cd $(top_builddir)/orte/tools/orte-clean && $(MAKE) $(AM_MAKEFLAGS) orte-clean.1)

ompi-clean.1: $(top_builddir)/orte/tools/orte-clean/orte-clean.1
	cp -f $(top_builddir)/orte/tools/orte-clean/orte-clean.1 ompi-clean.1

$(top_builddir)/orte/tools/orte-checkpoint/orte-checkpoint.1:
	(cd $(top_builddir)/orte/tools/orte-checkpoint && $(MAKE) $(AM_MAKEFLAGS) orte-checkpoint.1)

ompi-checkpoint.1: $(top_builddir)/orte/tools/orte-checkpoint/orte-checkpoint.1
	cp -f $(top_builddir)/orte/tools/orte-checkpoint/orte-checkpoint.1 ompi-checkpoint.1

$(top_builddir)/orte/tools/orte-restart/orte-restart.1:
	(cd $(top_builddir)/orte/tools/orte-restart && $(MAKE) $(AM_MAKEFLAGS) orte-restart.1)

ompi-restart.1: $(top_builddir)/orte/tools/orte-restart/orte-restart.1
	cp -f $(top_builddir)/orte/tools/orte-restart/orte-restart.1 ompi-restart.1

$(top_builddir)/orte/tools/orte-migrate/orte-migrate.1:
	(cd $(top_builddir)/orte/tools/orte-migrate && $(MAKE) $(AM_MAKEFLAGS) orte-migrate.1)

ompi-migrate.1: $(top_builddir)/orte/tools/orte-migrate/orte-migrate.1
	cp -f $(top_builddir)/orte/tools/orte-migrate/orte-migrate.1 ompi-migrate.1

$(top_builddir)/orte/tools/orte-top/orte-top.1:
	(cd $(top_builddir)/orte/tools/orte-top && $(MAKE) $(AM_MAKEFLAGS) orte-top.1)

ompi-top.1: $(top_builddir)/orte/tools/orte-top/orte-top.1
	cp -f $(top_builddir)/orte/tools/orte-top/orte-top.1 ompi-top.1

$(top_builddir)/orte/tools/orte-server/orte-server.1:
	(cd $(top_builddir)/orte/tools/orte-server && $(MAKE) $(AM_MAKEFLAGS) orte-server.1)

ompi-server.1: $(top_builddir)/orte/tools/orte-server/orte-server.1
	cp -f $(top_builddir)/orte/tools/orte-server/orte-server.1 ompi-server.1

ompi-dvm.1: $(top_builddir)/orte/tools/orte-dvm/orte-dvm.1
	cp -f $(top_builddir)/orte/tools/orte-dvm/orte-dvm.1 ompi-dvm.1

ompi-submit.1: $(top_builddir)/orte/tools/orte-submit/orte-submit.1
	cp -f $(top_builddir)/orte/tools/orte-submit/orte-submit.1 ompi-submit.1

clean-local:
	rm -f $(man_pages)

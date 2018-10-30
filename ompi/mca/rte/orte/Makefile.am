#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2018 Intel, Inc.  All rights reserved.
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

man_pages = mpirun.1 mpiexec.1 ompi-clean.1 ompi-server.1

if OPAL_INSTALL_BINARIES
nodist_man_MANS = $(man_pages)

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpirun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpiexec$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpiexec$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-clean$(EXEEXT); $(LN_S) orte-clean$(EXEEXT) ompi-clean$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-server$(EXEEXT); $(LN_S) orte-server$(EXEEXT) ompi-server$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun$(EXEEXT) \
		$(DESTDIR)$(bindir)/mpiexec$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-clean$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-server$(EXEEXT)

endif # OPAL_INSTALL_BINARIES

$(top_builddir)/orte/tools/orterun/orterun.1:
	(cd $(top_builddir)/orte/tools/orterun && $(MAKE) $(AM_MAKEFLAGS) orterun.1)

mpirun.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 mpirun.1

mpiexec.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 mpiexec.1

$(top_builddir)/orte/tools/orte-clean/orte-clean.1:
	(cd $(top_builddir)/orte/tools/orte-clean && $(MAKE) $(AM_MAKEFLAGS) orte-clean.1)

ompi-clean.1: $(top_builddir)/orte/tools/orte-clean/orte-clean.1
	cp -f $(top_builddir)/orte/tools/orte-clean/orte-clean.1 ompi-clean.1

$(top_builddir)/orte/tools/orte-server/orte-server.1:
	(cd $(top_builddir)/orte/tools/orte-server && $(MAKE) $(AM_MAKEFLAGS) orte-server.1)

ompi-server.1: $(top_builddir)/orte/tools/orte-server/orte-server.1
	cp -f $(top_builddir)/orte/tools/orte-server/orte-server.1 ompi-server.1

clean-local:
	rm -f $(man_pages)

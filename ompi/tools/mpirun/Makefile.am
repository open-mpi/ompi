#
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2021      Nanook Consulting.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if OMPI_WANT_PRRTE

bin_PROGRAMS = mpirun

mpirun_SOURCES = \
    main.c

mpirun_LDADD = \
	$(top_builddir)/opal/libopen-pal.la

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpiexec$(EXEEXT); $(LN_S) mpirun$(EXEEXT) mpiexec$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) oshrun$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun$(EXEEXT) \
		$(DESTDIR)$(bindir)/mpiexec$(EXEEXT)  \
		$(DESTDIR)$(bindir)/oshrun$(EXEEXT)
endif
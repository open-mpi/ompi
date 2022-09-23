#
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.
# Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
# Copyright (c) 2021      Nanook Consulting.  All rights reserved.
# Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if OMPI_WANT_PRRTE

bin_PROGRAMS = mpirun

dist_ompidata_DATA = help-mpirun.txt

mpirun_SOURCES = \
    main.c

mpirun_LDADD = \
	$(top_builddir)/opal/libopen-pal_core.la

mpirun_CPPFLAGS = \
    -DMCA_oshmem_FRAMEWORKS="\"$(MCA_oshmem_FRAMEWORKS)\"" \
    -DMCA_ompi_FRAMEWORKS="\"$(MCA_ompi_FRAMEWORKS)\"" \
    -DMCA_opal_FRAMEWORKS="\"$(MCA_opal_FRAMEWORKS)\""

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpiexec$(EXEEXT); $(LN_S) mpirun$(EXEEXT) mpiexec$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f oshrun$(EXEEXT); $(LN_S) mpirun$(EXEEXT) oshrun$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun$(EXEEXT) \
		$(DESTDIR)$(bindir)/mpiexec$(EXEEXT)  \
		$(DESTDIR)$(bindir)/oshrun$(EXEEXT)
endif

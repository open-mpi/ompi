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

dist_pkgdata_DATA = 

man_MANS =  mpirun.1 \
			mpiexec.1

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpirun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpiexec$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpiexec$(EXEEXT))

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun$(EXEEXT) \
		$(DESTDIR)$(bindir)/mpiexec$(EXEEXT)

mpirun.1: $(top_srcdir)/orte/tools/orterun/orterun.1
	cp -f $(top_srcdir)/orte/tools/orterun/orterun.1 mpirun.1

mpiexec.1: $(top_srcdir)/orte/tools/orterun/orterun.1
	cp -f $(top_srcdir)/orte/tools/orterun/orterun.1 mpiexec.1

distclean-local:
	rm -f mpirun.1 mpiexec.1 ompi-ps.1 ompi-clean.1

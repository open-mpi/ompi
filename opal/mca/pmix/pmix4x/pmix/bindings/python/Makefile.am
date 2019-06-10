#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

helpers = setup.py pmix.pyx

if WANT_PYTHON_BINDINGS

install-exec-local: $(helpers)
	$(PYTHON) $(top_srcdir)/bindings/python/construct.py --src="$(top_builddir)/include"
	$(PYTHON) $(top_srcdir)/bindings/python/setup.py build_ext --include-dirs="$(top_builddir)/include" --library-dirs="$(DESTDIR)$(libdir)" --user
	$(PYTHON) $(top_srcdir)/bindings/python/setup.py install --prefix="$(DESTDIR)$(prefix)"

uninstall-hook:
	rm -f $(pythondir)/pmix*.so
	rm -f $(pythondir)/pypmix-*.egg-info

CLEANFILES += pmix.c

clean-local:
	rm -rf build

endif

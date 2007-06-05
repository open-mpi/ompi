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
# Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Defaults
lib_LTLIBRARIES =
noinst_LTLIBRARIES =
include_HEADERS =
nodist_include_HEADERS =
noinst_HEADERS = plpa_internal.h
nodist_noinst_HEADERS =

nodist_public_headers = \
        plpa.h

public_headers = \
        plpa_bottom.h

# See which mode we're building in
if PLPA_BUILD_STANDALONE
lib_LTLIBRARIES += libplpa.la
include_HEADERS += $(public_headers)
nodist_include_HEADERS += $(nodist_public_headers)
else
noinst_LTLIBRARIES += libplpa_included.la
noinst_HEADERS += $(public_headers)
nodist_noinst_HEADERS += $(nodist_public_headers)
endif

# The sources
plpa_sources = \
        plpa_api_probe.c \
        plpa_dispatch.c \
        plpa_runtime.c \
        plpa_map.c

libplpa_la_SOURCES = $(plpa_sources)
libplpa_included_la_SOURCES = $(plpa_sources)

# Add a hook to run *after* the header file has been installed out to
# the target location.  It changes the pesky PACKAGE_* macros that
# autoconf automatically generates (and there is no way of turning
# off) into PLPA_PACKAGE_* in order to make <plpa.h> safe to include
# with other files.

if PLPA_BUILD_STANDALONE
install-data-hook:
	sed -e 's/define PACKAGE/define PLPA_PACKAGE/' \
		$(DESTDIR)$(includedir)/plpa.h \
		> $(DESTDIR)$(includedir)/plpa.h.install
	cp $(DESTDIR)$(includedir)/plpa.h.install \
		$(DESTDIR)$(includedir)/plpa.h
	rm -f $(DESTDIR)$(includedir)/plpa.h.install
	chmod 0644 $(DESTDIR)$(includedir)/plpa.h
endif

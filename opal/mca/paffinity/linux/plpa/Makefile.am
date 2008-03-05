#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = src
DIST_SUBDIRS = config $(SUBDIRS)
EXTRA_DIST = README VERSION LICENSE AUTHORS autogen.sh

if PLPA_BUILD_STANDALONE
dist-hook:
	csh "$(top_srcdir)/config/distscript.csh" "$(top_srcdir)" "$(distdir)" "$(PLPA_VERSION)" "$(PLPA_SVN_R)"
endif

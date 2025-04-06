#
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# This Makefile exists (as opposed to being part of the top level
# Makefile) solely to have a blank check-recursive rule.  Otherwise, the
# stability of Open MPI's ability to run "make check" or "make
# distcheck" is dependent on the ability of our 3rd-party packages to
# do the same.  Libevent's tests are not stable enough for that kind
# of choice.

SUBDIRS = $(OPAL_3RDPARTY_SUBDIRS)
DIST_SUBDIRS = $(OPAL_3RDPARTY_DIST_SUBDIRS)
EXTRA_DIST =  $(OPAL_3RDPARTY_EXTRA_DIST) autogen.subdirs pympistandard

distclean-local:
	rm -rf $(OPAL_3RDPARTY_DISTCLEAN_DIRS)

check:
	@echo "auto-recursing into 3rd-party packages for check disabled"

check-recursive:
	@echo "auto-recursing into 3rd-party packages for check disabled"

# We recursively copied the pympystandard directory.  As suggested by
# the Automake docs (see section 14.3, "The dist Hook"), use the
# dist-hook target to delete a bunch of extra stuff that we might have
# copied.
dist-hook:
	@find $(distdir)/pympistandard -name __pycache__ -exec rm -rf {} \; || :
	@find $(distdir)/pympistandard -name \*.pyc -exec rm -rf {} \; || :
	@rm -rf $(distdir)/pympistandard/.git

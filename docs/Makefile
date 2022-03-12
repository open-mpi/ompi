#
# Copyright (c) 2022 Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This directory is not included in Open MPI distribution tarballs.
# Its only purpose is to be built by readthedocs.io in order to show a
# skeleton "This version of Open MPI does not have RTD docs / see
# these other places for docs for this version of Open MPI".

OUTDIR             = _build
SPHINX_CONFIG      = conf.py
SPHINX_OPTS       ?= -W --keep-going

all:
	sphinx-build -M html "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

.PHONY: all

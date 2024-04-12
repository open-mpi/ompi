#
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# we do NOT want picky compilers down here
CFLAGS = $(PMIX_CFLAGS_BEFORE_PICKY)

noinst_SCRIPTS = \
	run_server.sh.in \
	run_sched.sh.in \
    server.py \
    sched.py \
    client.py

#########################
# Support for "make check"

if WANT_PYTHON_BINDINGS

TESTS = \
    run_server.sh \
    run_sched.sh

endif

# -*- makefile -*-
#
# Copyright (c) 2021 IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This is not quite in the Automake spirit, but we have to do it.
# Since the mpir portion of the library must be built with -g, we
# must eliminate the CFLAGS that are passed in here by default (which
# may already have debugging and/or optimization flags).

CFLAGS = $(CFLAGS_WITHOUT_OPTFLAGS) $(DEBUGGER_CFLAGS)

noinst_LTLIBRARIES = lib@ORTE_LIB_PREFIX@open-orted-mpir.la
lib@ORTE_LIB_PREFIX@open_orted_mpir_la_SOURCES = \
        orted_mpir_breakpoint.c \
        orted_mpir.h
lib@ORTE_LIB_PREFIX@open_orted_mpir_la_LDFLAGS = -avoid-version

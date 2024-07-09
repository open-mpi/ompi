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
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AM_LFLAGS = -Popal_util_keyval_yy
LEX_OUTPUT_ROOT = lex.opal_util_keyval_yy

noinst_LTLIBRARIES = libopalutilkeyval.la

# Source code files

libopalutilkeyval_la_SOURCES = \
	keyval_lex.h \
        keyval_lex.l

# flex prior to version 2.6.6 uses the POSIX extension fileno()
# without providing the proper feature test macro, so
# we add the _POSIX_C_SOURCE macro here.
# See https://github.com/westes/flex/issues/263
libopalutilkeyval_la_CFLAGS = -D_POSIX_C_SOURCE=200809L

maintainer-clean-local:
	rm -f keyval_lex.c

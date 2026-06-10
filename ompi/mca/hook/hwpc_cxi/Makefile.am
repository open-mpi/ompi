#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier:  MIT
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	hook_hwpc_cxi.h \
	hook_hwpc_cxi_component.c \
	hook_hwpc_cxi_constants.h \
	hook_hwpc_cxi_constants.c \
	hook_hwpc_cxi_counters.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_hook_hwpc_cxi.la

libmca_hook_hwpc_cxi_la_SOURCES = $(sources)
libmca_hook_hwpc_cxi_la_LDFLAGS = -module -avoid-version

#
# Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# We only ever build this component statically

noinst_LTLIBRARIES = libmca_event_external.la
libmca_event_external_la_SOURCES = \
        external.h \
        event_external_component.c \
        event_external_module.c
libmca_event_external_la_CPPFLAGS = $(opal_event_external_CPPFLAGS)

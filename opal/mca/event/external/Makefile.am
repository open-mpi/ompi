#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
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

#Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = external.h
endif

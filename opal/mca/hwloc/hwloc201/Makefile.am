#
# Copyright (c) 2011-2018 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Due to what might be a bug in Automake, we need to remove stamp-h?
# files manually.  See
# http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19418.
DISTCLEANFILES = \
    hwloc/include/hwloc/autogen/stamp-h? \
    hwloc/include/private/autogen/stamp-h?

# Need to include these files so that these directories are carried in
# the tarball (in case someone invokes autogen.sh on a dist tarball).
EXTRA_DIST = \
        hwloc/doc/README.txt \
        hwloc/contrib/systemd/README.txt \
        hwloc/tests/README.txt \
        hwloc/utils/README.txt \
        hwloc/netloc/README.txt \
        hwloc/contrib/misc/README.txt

SUBDIRS = hwloc

# Headers and sources
headers = hwloc201.h
sources = hwloc201_component.c

# We only ever build this component statically
noinst_LTLIBRARIES = libmca_hwloc_hwloc201.la
libmca_hwloc_hwloc201_la_SOURCES = $(headers) $(sources)
nodist_libmca_hwloc_hwloc201_la_SOURCES = $(nodist_headers)
libmca_hwloc_hwloc201_la_LDFLAGS = -module -avoid-version $(opal_hwloc_hwloc201_LDFLAGS)
libmca_hwloc_hwloc201_la_LIBADD = $(opal_hwloc_hwloc201_LIBS)
libmca_hwloc_hwloc201_la_DEPENDENCIES = \
    $(HWLOC_top_builddir)/hwloc/libhwloc_embedded.la

# Since the rest of the code base includes the underlying hwloc.h, we
# also have to install the underlying header files when
# --with-devel-headers is specified.  hwloc doesn't support this; the
# least gross way to make this happen is just to list all of hwloc's
# header files here.  :-(
headers += \
        hwloc/include/hwloc.h \
        hwloc/include/hwloc/bitmap.h \
        hwloc/include/hwloc/cuda.h \
        hwloc/include/hwloc/cudart.h \
        hwloc/include/hwloc/deprecated.h \
        hwloc/include/hwloc/diff.h \
        hwloc/include/hwloc/distances.h \
        hwloc/include/hwloc/export.h \
        hwloc/include/hwloc/gl.h \
        hwloc/include/hwloc/helper.h \
        hwloc/include/hwloc/inlines.h \
        hwloc/include/hwloc/intel-mic.h \
        hwloc/include/hwloc/nvml.h \
        hwloc/include/hwloc/opencl.h \
        hwloc/include/hwloc/openfabrics-verbs.h \
        hwloc/include/hwloc/plugins.h \
        hwloc/include/hwloc/rename.h \
        hwloc/include/hwloc/shmem.h \
        hwloc/include/private/private.h \
        hwloc/include/private/debug.h \
        hwloc/include/private/misc.h \
        hwloc/include/private/cpuid-x86.h
nodist_headers = hwloc/include/hwloc/autogen/config.h

if HWLOC_HAVE_LINUX
headers += \
        hwloc/include/hwloc/linux.h \
        hwloc/include/hwloc/linux-libnuma.h
endif HWLOC_HAVE_LINUX

if HWLOC_HAVE_SOLARIS
headers += \
        hwloc/include/private/solaris-chiptype.h
endif HWLOC_HAVE_SOLARIS

if HWLOC_HAVE_SCHED_SETAFFINITY
headers += hwloc/include/hwloc/glibc-sched.h
endif HWLOC_HAVE_SCHED_SETAFFINITY

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
nobase_nodist_opal_HEADERS = $(nodist_headers)
endif

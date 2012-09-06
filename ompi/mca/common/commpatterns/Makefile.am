#
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# A word of explanation...
#
# This library is linked against various MCA components because all
# shared-memory based components (e.g., mpool, ptl, etc.)  need to
# share some common code and data.  There's two cases:
#
# 1. libmca_common_commpatterns.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
#
# 2. libmca_common_commpatterns.la is a static library.  In this case, it will
# be rolled up into the top-level libmpi.la.  It will also be rolled
# into each component, but then the component will also be rolled up
# into the upper-level libmpi.la.  Linkers universally know how to
# "figure this out" so that we end up with only one copy of the code
# and data.
#
# Note that building this common component statically and linking
# against other dynamic components is *not* supported!

EXTRA_DIST = .windows

# Header files

headers = \
        common_coll_ops.h \
        common_netpatterns.h \
        ompi_common_netpatterns_macros.h

# Source files

sources = \
        common_allreduce.c \
        common_allgather.c \
        common_bcast.c

# As per above, we'll either have an installable or noinst result.
# The installable one should follow the same MCA prefix naming rules
# (i.e., libmca_<type>_<name>.la).  The noinst one can be named
# whatever it wants, although libmca_<type>_<name>_noinst.la is
# recommended.

# To simplify components that link to this library, we will *always*
# have an output libtool library named libmca_<type>_<name>.la -- even
# for case 2) described above (i.e., so there's no conditional logic
# necessary in component Makefile.am's that link to this library).
# Hence, if we're creating a noinst version of this library (i.e.,
# case 2), we sym link it to the libmca_<type>_<name>.la name
# (libtool will do the Right Things under the covers).  See the
# all-local and clean-local rules, below, for how this is effected.

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = libmca_common_commpatterns.la
comp_noinst = libmca_common_commpatterns_noinst.la

if MCA_BUILD_ompi_common_commpatterns_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

libmca_common_commpatterns_la_SOURCES = $(headers) $(sources)
libmca_common_commpatterns_noinst_la_SOURCES = $(libmca_common_commpatterns_la_SOURCES)
libmca_common_commpatterns_la_LIBADD = \
		$(top_ompi_builddir)/ompi/mca/common/netpatterns/libmca_common_netpatterns.la

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).

all-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi
# The code below guaranty that the netpatterns will be build before commpatterns
FORCE:

$(top_ompi_builddir)/ompi/mca/common/netpatterns/libmca_common_netpatterns.la: FORCE
	(cd  $(top_ompi_builddir)/ompi/mca/common/netpatterns/ && $(MAKE) $(AM_MAKEFLAGS) libmca_common_netpatterns.la)

install-libmca_common_netpatterns: FORCE
	(cd  $(top_ompi_builddir)/ompi/mca/common/netpatterns/ && $(MAKE) $(AM_MAKEFLAGS) install)

install: install-libmca_common_netpatterns install-am


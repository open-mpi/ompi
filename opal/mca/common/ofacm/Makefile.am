#
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(common_ofacm_CPPFLAGS)

dist_opaldata_DATA = \
        help-mpi-common-ofacm-base.txt \
        help-mpi-common-ofacm-oob.txt
headers = \
	base.h \
	common_ofacm_oob.h \
	common_ofacm_empty.h \
	connect.h

sources = \
    common_ofacm_base.c \
    common_ofacm_oob.c \
    common_ofacm_empty.c

# If we have XRC support, build XOOB connection module
if MCA_common_ofacm_have_xrc
sources += \
    common_ofacm_xoob.c \
    common_ofacm_xoob.h
endif

# See opal/mca/common/sm/Makefile.am for an explanation of the
# LTLIBRARIES values listed below.

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = libmca_common_ofacm.la
comp_noinst = libmca_common_ofacm_noinst.la

if MCA_BUILD_opal_common_ofacm_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

libmca_common_ofacm_la_SOURCES = $(headers) $(sources)
libmca_common_ofacm_la_CPPFLAGS = $(common_ofacm_CPPFLAGS)
libmca_common_ofacm_la_LDFLAGS = \
        -version-info $(libmca_common_ofacm_so_version) \
        $(common_ofacm_LDFLAGS)
libmca_common_ofacm_la_LIBADD = $(common_ofacm_LIBS)
libmca_common_ofacm_noinst_la_SOURCES = $(libmca_common_ofacm_la_SOURCES)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/opal/mca/common/ofacm
opal_HEADERS = $(headers)
else
opaldir = $(includedir)
endif

# See opal/mca/common/sm/Makefile.am for an explanation of the
# the *-local rules, below.

V=0
OMPI_V_LN_SCOMP = $(ompi__v_LN_SCOMP_$V)
ompi__v_LN_SCOMP_ = $(ompi__v_LN_SCOMP_$AM_DEFAULT_VERBOSITY)
ompi__v_LN_SCOMP_0 = @echo "  LN_S    " `basename $(comp_inst)`;

all-local:
	$(OMPI_V_LN_SCOMP) if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi

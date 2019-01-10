#
# Copyright (c) 2018      Mellanox Technologies.  All rights reserved.
#
# Copyright (c) 2018      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Header files

AM_CPPFLAGS = $(LTDLINCL)

headers = \
        dstore_common.h \
        dstore_base.h \
        dstore_segment.h \
        dstore_file.h

# Source files

sources = \
        dstore_base.c \
        dstore_segment.c

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = libmca_common_dstore.la
comp_noinst = libmca_common_dstore_noinst.la

if MCA_BUILD_pmix_common_dstore_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

libmca_common_dstore_la_SOURCES = $(headers) $(sources)
libmca_common_dstore_la_LDFLAGS = -version-info $(libmca_common_dstore_so_version)
libmca_common_dstore_noinst_la_SOURCES = $(headers) $(sources)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
pmixdir = $(pmixincludedir)/$(subdir)
pmix_HEADERS = $(headers)
endif

all-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
		rm -f "$(comp_inst)"; \
		$(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
		rm -f "$(comp_inst)"; \
	fi

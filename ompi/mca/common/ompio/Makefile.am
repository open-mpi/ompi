#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2018 University of Houston. All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2018 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

headers = \
	common_ompio_aggregators.h \
	common_ompio_print_queue.h \
	common_ompio_request.h \
	common_ompio.h

sources = \
	common_ompio_aggregators.c \
	common_ompio_print_queue.c \
	common_ompio_request.c \
	common_ompio_file_open.c   \
	common_ompio_file_view.c   \
	common_ompio_file_read.c   \
	common_ompio_file_write.c


# See the comments in opal/mca/common/sm/Makefile.am for the explanations

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = libmca_common_ompio.la
comp_noinst = libmca_common_ompio_noinst.la

if MCA_BUILD_ompi_common_ompio_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

libmca_common_ompio_la_SOURCES = $(headers) $(sources)
libmca_common_ompio_la_CPPFLAGS = $(common_ompio_CPPFLAGS)
libmca_common_ompio_la_LDFLAGS =  \
        -version-info $(libmca_ompi_common_ompio_so_version) \
        $(common_ompio_LDFLAGS)
libmca_common_ompio_la_LIBADD = $(common_ompio_LIBS)
libmca_common_ompio_noinst_la_SOURCES = $(headers) $(sources)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/ompi/mca/common/ompio
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

if OPAL_cuda_support
headers += common_ompio_cuda.h
sources += common_ompio_cuda.c
endif

all-local: $(comp_noinst)
	$(OMPI_V_LN_SCOMP) if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi

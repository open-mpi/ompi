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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from opal/Makefile.am

# Source code files
OMPI_WIN32_SRC = \
	win32/ompi_utsname.c \
	win32/ompi_process.c \
	win32/ompi_time.c \
	win32/ompi_uio.c

OMPI_WIN32_HDR = \
	win32/ompi_misc.h \
	win32/ompi_process.h \
	win32/ompi_time.h \
	win32/ompi_uio.h \
	win32/ompi_util.h \
	win32/ompi_utsname.h \
	win32/win_compat.h

if OMPI_NEED_WINDOWS_REPLACEMENTS
libopal_la_SOURCES += $(OMPI_WIN32_SRC)
headers += $(OMPI_WIN32_HDR)
endif

# Always include Windows files in dist
EXTRA_DIST += $(OMPI_WIN32_HDR) \
              $(OMPI_WIN32_SRC) \
              win32/generated_include/allocator_static-components.h  \
              win32/generated_include/iof_static-components.h   \
              win32/generated_include/errmgr_static-components.h \
              win32/generated_include/rml_static-components.h   \
              win32/generated_include/rmgr_static-components.h  \
              win32/generated_include/schema_static-components.h  \
              win32/generated_include/rmaps_static-components.h \
              win32/generated_include/soh_static-components.h \
              win32/generated_include/ns_static-components.h    \
              win32/generated_include/pcmclient-seed-version.h  \
              win32/generated_include/pml_static-components.h   \
              win32/generated_include/coll-basic-version.h      \
              win32/generated_include/io-romio-version.h        \
              win32/generated_include/ompi_config.h             \
              win32/generated_include/pcmclient-singleton-version.h  \
              win32/generated_include/ptl-prof-version.h        \
              win32/generated_include/coll_static-components.h  \
              win32/generated_include/oob_static-components.h   \
              win32/generated_include/pcmclient-slurm-version.h \
              win32/generated_include/ptl_static-components.h   \
              win32/generated_include/config.h                  \
              win32/generated_include/llm-hostfile-version.h    \
              win32/generated_include/pcmclient-env-version.h   \
              win32/generated_include/pcmclient_static-components.h  \
              win32/generated_include/topo_static-components.h  \
              win32/generated_include/gpr_static-components.h   \
              win32/generated_include/llm_static-components.h   \
              win32/generated_include/pcmclient-rms-version.h   \
              win32/generated_include/pcm_static-components.h   \
              win32/generated_include/topo-unity-version.h      \
              win32/generated_include/pls_static-components.h \
              win32/generated_include/ras_static-components.h \
              win32/generated_include/rds_static-components.h \
              win32/generated_include/pls-fork-version.h \
              win32/generated_include/pls-proxy-version.h \
              win32/generated_include/pls-rsh-version.h \
              win32/generated_include/libltdl/ltdl.h \
              win32/generated_source/ltdl.c \
              win_makefile


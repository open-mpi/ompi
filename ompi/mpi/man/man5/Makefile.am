# -*- makefile -*-
#
# Copyright (c) 2020 Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# Just in case someone looks for it here someday, here is a
# conveninent reference for what Markdown pandoc supports:
#
# https://rmarkdown.rstudio.com/authoring_pandoc_markdown.html
MD_FILES = \
        MPI_T.5.md \
        Open-MPI.5.md

native_nroff_files = \
        MPI.5 \
        OpenMPI.5

man_pages_from_md = $(MD_FILES:.5.md=.5)

if OPAL_ENABLE_MAN_PAGES
man_MANS = $(man_pages_from_md) $(native_nroff_files)
MAINTAINERCLEANFILES = $(man_pages_from_md)
endif

EXTRA_DIST = $(MD_FILES) $(man_pages_from_md) $(native_nroff_files)

#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options

# The majority of stuff to build the PS/PDF files are included in a
# separate Makefile so that they can be invoked directly at "make
# dist" time without needing to generate a real "Makefile" from
# Makefile.am.

include Makefile.latex

# Now ensure that all the relevant files get picked up in the tarball.

EXTRA_DIST      = $(MAIN_TEX) $(OTHER_SRC_FILES) $(FIG_FILES) $(PNG_FILES) $(BIBTEX_SOURCES) Makefile.latex

dist-hook: pdf
	mv user.pdf $(DESTDIR)$(distdir)/../.

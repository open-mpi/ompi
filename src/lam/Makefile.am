#
# $HEADER$
#
# Copyright 2002-2003. The Regents of the University of California. This material
# was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
# National Laboratory, which is operated by the University of California for
# the U.S. Department of Energy. The Government is granted for itself and
# others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
# license in this material to reproduce, prepare derivative works, and
# perform publicly and display publicly. Beginning five (5) years after
# October 10,2002 subject to additional five-year worldwide renewals, the
# Government is granted for itself and others acting on its behalf a paid-up,
# nonexclusive, irrevocable worldwide license in this material to reproduce,
# prepare derivative works, distribute copies to the public, perform publicly
# and display publicly, and to permit others to do so. NEITHER THE UNITED
# STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
# CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
# IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
# COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
# PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
# OWNED RIGHTS.
#
# Additionally, this program is free software; you can distribute it and/or
# modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2 of the License,
# or any later version.  Accordingly, this program is distributed in the hope
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS	= ctnetwork lfc mem os threads util

# If the --enable-single-library flag was given to configure, then the
# user wants to merge liblam and libmpi into a single big, honkin'
# libmpi.  So the libmpi in this directory should be a convenience
# (noinst) library.  Otherwise, it should be installed into lib.

# List the input convenience libraries here

sources 		= \
			ctnetwork/libctnetwork.la \
			lfc/liblfc.la \
			mem/libmem.la \
			threads/libthreads.la \
			util/libutil.la 

# List the names of the output libraries here

convenience_lib_name	= liblam_convenience.la
install_lib_name	= liblam.la

# Setup for deciding which library form we'll build

if WANT_SINGLE_MPI_LIBRARY
install_lib		=
convenience_lib		= $(convenience_lib_name)
else
install_lib		= $(install_lib_name)
convenience_lib		=
endif

lib_LTLIBRARIES		= $(install_lib)
noinst_LTLIBRARIES	= $(convenience_lib)

liblam_la_SOURCES	=
liblam_la_LIBADD	= $(sources)

liblam_convenience_la_SOURCES =
liblam_convenience_la_LIBADD = $(sources)


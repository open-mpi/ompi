#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_pml_v_CONFIG],[
  protocol_subdirs=""
  while read protocol; do 
    echo "Adding $protocol to pml_v subdirs"
    protocol_subdirs="$protocol $protocol_subdirs"
  done <$srcdir/ompi/mca/pml/v/autogen.vprotocols
  
  AC_SUBST(MCA_pml_v_PROTOCOLS, [$protocol_subdirs])
])

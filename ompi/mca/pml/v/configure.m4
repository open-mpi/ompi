AC_DEFUN([MCA_pml_v_CONFIG],[
  protocol_subdirs=""
  while read protocol; do 
    echo "Adding $protocol to pml_v subdirs"
    protocol_subdirs="$protocol $protocol_subdirs"
  done <ompi/mca/pml/v/autogen.vprotocols
  
  AC_SUBST(MCA_pml_v_PROTOCOLS, [$protocol_subdirs])
])

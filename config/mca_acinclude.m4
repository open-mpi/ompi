dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl
dnl Tests provided by OMPI
dnl General tests
dnl

sinclude(@M4DIR@/ompi_functions.m4)
sinclude(@M4DIR@/ompi_get_version.m4)

dnl
dnl C compiler tests
dnl

sinclude(@M4DIR@/ompi_setup_cc.m4)
sinclude(@M4DIR@/ompi_check_optflags.m4)

dnl
dnl C++ compiler tests
dnl

sinclude(@M4DIR@/ompi_setup_cxx.m4)
sinclude(@M4DIR@/cxx_find_template_repository.m4)
sinclude(@M4DIR@/cxx_find_template_parameters.m4)

dnl
dnl This will be replaced with s_i_n_c_l_u_d_e(configure.stub) if it
dnl exists for that component, or a blank line if it does not.
dnl

@CONFIGURE_STUB_SINCLUDE@

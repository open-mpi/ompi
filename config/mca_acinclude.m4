dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

dnl
dnl Tests provided by OMPI
dnl General tests
dnl

sinclude(../../../../config/ompi_functions.m4)
sinclude(../../../../config/ompi_get_version.m4)

dnl
dnl C compiler tests
dnl

sinclude(../../../../config/ompi_setup_cc.m4)
sinclude(../../../../config/ompi_check_optflags.m4)

dnl
dnl C++ compiler tests
dnl

sinclude(../../../../config/ompi_setup_cxx.m4)
sinclude(../../../../config/cxx_find_template_repository.m4)
sinclude(../../../../config/cxx_find_template_parameters.m4)

dnl
dnl This will be replaced with s_i_n_c_l_u_d_e(configure.stub) if it
dnl exists for that component, or a blank line if it does not.
dnl

@CONFIGURE_STUB_SINCLUDE@

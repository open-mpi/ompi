dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

dnl
dnl Tests provided by LAM
dnl General tests
dnl

sinclude(../../../../../config/lam_functions.m4)
sinclude(../../../../../config/lam_get_version.m4)

dnl
dnl C compiler tests
dnl

sinclude(../../../../../config/lam_setup_cc.m4)
sinclude(../../../../../config/lam_check_optflags.m4)

dnl
dnl C++ compiler tests
dnl

sinclude(../../../../../config/lam_setup_cxx.m4)
sinclude(../../../../../config/cxx_find_template_repository.m4)
sinclude(../../../../../config/cxx_find_template_parameters.m4)

dnl
dnl Macro to allow SSI configure.stub files to [effectively] sinclude
dnl other m4 files
dnl

dnl JMS Need to fix this somehow -- AM 1.8 doesn't like it
dnl define([LAM_INCLUDE], [sinclude($1)])dnl

dnl
dnl This will be replaced with sinclude(configure.stub) if it exists
dnl for that module, or a blank line if it does not.
dnl

@CONFIGURE_STUB_SINCLUDE@

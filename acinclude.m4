dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

#
# LAM/MPI-specific tests
#

sinclude(config/cxx_find_template_parameters.m4)
sinclude(config/cxx_find_template_repository.m4)

sinclude(config/lam_check_optflags.m4)
sinclude(config/lam_configure_options.m4)
sinclude(config/lam_functions.m4)
sinclude(config/lam_get_version.m4)
sinclude(config/lam_setup_cc.m4)
sinclude(config/lam_setup_cxx.m4)

#
# Contributed tests
#

sinclude(config/ax_create_stdint_h.m4)

dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

#
# LAM/MPI-specific tests
#

sinclude(config/c_weak_symbols.m4)

sinclude(config/cxx_find_template_parameters.m4)
sinclude(config/cxx_find_template_repository.m4)
sinclude(config/cxx_have_exceptions.m4)
sinclude(config/cxx_find_exception_flags.m4)

sinclude(config/lam_check_optflags.m4)
sinclude(config/lam_configure_options.m4)
sinclude(config/lam_functions.m4)
sinclude(config/lam_get_version.m4)
sinclude(config/lam_setup_cc.m4)
sinclude(config/lam_setup_cxx.m4)
sinclude(config/lam_setup_f77.m4)
sinclude(config/lam_setup_f90.m4)

#
# Contributed tests
# JMS This needs to be removed
#

sinclude(config/ax_create_stdint_h.m4)

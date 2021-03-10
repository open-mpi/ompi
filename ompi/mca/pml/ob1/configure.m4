# -*- shell-script -*-
#
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_pml_ob1_POST_CONFIG(will_build)
# ----------------------------------------
# The OB1 PML requires a BML endpoint tag to compile, so require it.
# Require in POST_CONFIG instead of CONFIG so that we only require it
# if we're not disabled.
AC_DEFUN([MCA_ompi_pml_ob1_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([BML])])
])dnl

# MCA_ompi_pml_ob1_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
# We can always build, unless we were explicitly disabled.
AC_DEFUN([MCA_ompi_pml_ob1_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([pml_ob1_matching_engine])
    AC_ARG_WITH([pml-ob1-matching], [AS_HELP_STRING([--with-pml-ob1-matching=type],
                                                    [Configure pml/ob1 to use an alternate matching engine. Only valid on x86_64 systems.
                                                     Valid values are: none, default, arrays, fuzzy-byte, fuzzy-short, fuzzy-word, vector (default: none)])])

    pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_NONE

    if test -n "$with_pml_ob1_matching" ; then
        case $with_pml_ob1_matching in
            none)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_NONE
                ;;
            default)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_LINKEDLIST
                ;;
            arrays)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_ARRAYS
                ;;
            fuzzy-byte)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_BYTE
                ;;
            fuzzy-short)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_SHORT
                ;;
            fuzzy-word)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_WORD
                ;;
            vector)
                pml_ob1_matching_engine=MCA_PML_OB1_CUSTOM_MATCHING_VECTOR
                ;;
            *)
                AC_MSG_ERROR([invalid matching type specified for --pml-ob1-matching: $with_pml_ob1_matching])
                ;;
        esac
    fi

    AC_DEFINE_UNQUOTED([MCA_PML_OB1_CUSTOM_MATCHING], [$pml_ob1_matching_engine], [Custom matching engine to use in pml/ob1])

    AC_CONFIG_FILES([ompi/mca/pml/ob1/Makefile])
    [$1]
])dnl

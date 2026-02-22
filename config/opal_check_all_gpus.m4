dnl
dnl Copyright (C) 2026 Advanced Micro Devices, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OMPI_CHECK_ALL_GPUS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if any of the supported GPU types are supported.
# This macro does set any env, just returns a succes/failure
# result.


#
# Check for any GPU type support. Will have to be adjusted if any new GPU
# type is added to Open MPI
#
AC_DEFUN([OPAL_CHECK_ALL_GPUS],[
     OPAL_VAR_SCOPE_PUSH([opal_check_allgpus_happy])

     OPAL_CHECK_CUDA([allgpus_cuda],
	             [opal_allgpus_cuda_support="yes"],
	             [opal_allgpus_cuda_support="no"])

     OPAL_CHECK_ROCM([allgpus_rocm],
	             [opal_allgpus_rocm_support="yes"],
	             [opal_allgpus_rocm_support="no"])

     OPAL_CHECK_ZE([allgpus_ze],
	             [opal_allgpus_ze_support="yes"],
	             [opal_allgpus_ze_support="no"])

     AS_IF([test "$opal_allgpus_rocm_support" = "yes" ||
            test "$opal_allgpus_cuda_support" = "yes" ||
	    test "$opal_allgpus_ze_support" = "yes"],
           [$2],
	   [AC_MSG_WARN([No GPU support detected])
	   $3])

     OPAL_VAR_SCOPE_POP
])

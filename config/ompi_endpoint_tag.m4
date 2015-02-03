dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_REQUIRE_ENDPOINT_TAG_INIT], [
    ompi_endpoint_tag_counter=0
])


# OMPI_REQUIRE_ENDPOINT_TAG(tag_name)
# -----------------------------------
# This component requires an endpoint tag (storage space in the
# ompi_proc_t structure) for a tag named tag_name.  If tag_name
# already has a tag index, this function is a no-op.
AC_DEFUN([OMPI_REQUIRE_ENDPOINT_TAG], [
    AC_REQUIRE([OMPI_REQUIRE_ENDPOINT_TAG_INIT])
    AC_MSG_CHECKING([for index in endpoint array for tag $1])
    AS_IF([test -z "$OMPI_PROC_ENDPOINT_TAG_$1"],
          [OMPI_PROC_ENDPOINT_TAG_$1=$ompi_endpoint_tag_counter
           AC_DEFINE_UNQUOTED([OMPI_PROC_ENDPOINT_TAG_$1], [$OMPI_PROC_ENDPOINT_TAG_$1],
             [Index into endpoint array for $1])
           ompi_endpoint_tag_counter=`expr $ompi_endpoint_tag_counter + 1`])
    AC_MSG_RESULT([$OMPI_PROC_ENDPOINT_TAG_$1])
])


AC_DEFUN([OMPI_REQUIRE_ENDPOINT_TAG_FINI], [
dnl    AC_ARG_ENABLE([dynamic-endpoint-information],
dnl      [AC_HELP_STRING([--disable-dynamic-endpoint-information],
dnl         [Disable support for dynamic endpoint information storage.  This saves a considerable amount of memory for large processes, but makes loading outside components which require endpoint storage impossible.])])
dnl    AS_IF([test "$enable_endpoint_information" != "no"],
dnl          [OMPI_REQUIRE_ENDPOINT_TAG(DYNAMIC)])

    AC_MSG_CHECKING([for size of endpoint array])
    AS_IF([test -z "$ompi_endpoint_tag_counter" || test "$ompi_endpoint_tag_counter" = "0"],
          [AC_MSG_ERROR([Endpoint index count is 0.  This means no MPI communication would be possible.  Aborting.])])
    AC_MSG_RESULT([$ompi_endpoint_tag_counter])
    AC_DEFINE_UNQUOTED([OMPI_PROC_ENDPOINT_TAG_MAX], [$ompi_endpoint_tag_counter],
      [Maximum number of endpoint entries to be attached to an ompi_proc_t])
])

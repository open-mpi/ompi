dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2021 IBM Corporation.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


dnl OPAL_CHECK_PKG_CONFIG
dnl Check for availability of pkg-config and store the result.
dnl If it is not available, store any passed in libs from the
dnl --with-extra-libs configure option, or the known defaults.
dnl
dnl If it is available, allow configury to check for .pc files
dnl and append to OPAL_WRAPPER_EXTRA_LIBS.
AC_DEFUN([OPAL_CHECK_PKG_CONFIG], [
  AS_IF([test -z "$OPAL_HAS_PKG_CONFIG"],
        [
          AC_MSG_CHECKING([If pkg-config is available])
          which pkg-config > /dev/null 2>&1
          AS_IF([test "$?" -eq "0"],
                [
                  AC_MSG_RESULT([yes])
                  OPAL_HAS_PKG_CONFIG="yes"
                ],
                [
                  AC_MSG_RESULT([no])
                  OPAL_HAS_PKG_CONFIG="no"
                  OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_EXTRA_LIBS], [$OPAL_EXTRA_LIBS])
                ]
          )
        ]
  )
])

dnl OPAL_GET_LFLAGS_FROM_PC
dnl Get the -l flags using pkg-config on the passed in .pc file, and
dnl append them to OPAL_WRAPPER_EXTRA_LIBS.
dnl
dnl Requires 1 or more arguments.
dnl
dnl Arg 1 (Required): the name of the .pc file to look for (hwloc/pmix/libevent_core).pc
dnl 
dnl Arg 2..n (Optional): The search path for the .pc file above and any dependencies.
dnl                      These search will be appended and set in the env variable
dnl                      PKG_CONFIG_PATH to tell pkg-config where to locate the .pc
dnl                      file from argument 1, and its dependencies.
dnl
AC_DEFUN([OPAL_GET_LFLAGS_FROM_PC], [

  AC_REQUIRE([OPAL_CHECK_PKG_CONFIG])
  AS_IF([test "$OPAL_HAS_PKG_CONFIG" = "yes"],
        [
          AC_MSG_NOTICE([Looking for pc file for $1])
          OPAL_VAR_SCOPE_PUSH([opal_pkg_config_name opal_pkg_config_env_to_set opal_pkg_config_args opal_pkg_config_cmd opal_pkg_config_result])
          opal_pkg_config_name=$1
          opal_pkg_config_env_to_set=""
          opal_pkg_config_cmd=""

          AS_LITERAL_WORD_IF([$1], [], [m4_fatal([Non-literal argument $1])], [])
          $1_OMPI_PC_DIR=""

          # Tell pkg-config where to find the .pc file from argument 1
          # as well as any/all dependencies.
          # The following block will append the path
          # to those .pc files into the pkg-config command.
          # num_paths > 1 implies there is at least one dependent .pc file
          # to append.
          AS_IF([test $# -gt 1],
                [
                  $1_OMPI_PC_DIR=$2

                  # Shift the arguments by one to get to the actual paths.
                  opal_pkg_config_args=m4_shift($@)

                  # Iterate over the comma seperated arguments, and replace the ','
                  for i in $(echo $opal_pkg_config_args | sed "s/,/ /g"); do
                    opal_pkg_config_env_to_set="$i:${opal_pkg_config_env_to_set}"
                  done
                  opal_pkg_config_cmd="env PKG_CONFIG_PATH=$opal_pkg_config_env_to_set pkg-config --libs $opal_pkg_config_name"
                ],
                [opal_pkg_config_cmd="pkg-config --libs $opal_pkg_config_name"]
          )

          OPAL_LOG_MSG([pkg-config cmd for $opal_pkg_config_name: $opal_pkg_config_cmd])

          opal_pkg_config_result=$($opal_pkg_config_cmd)
          AS_IF([test -z "$opal_pkg_config_result"],
                [
                  AC_MSG_WARN([Could not find viable $opal_pkg_config_name.pc])
                ],
                [
                  AC_MSG_NOTICE([pkg-config result $opal_pkg_config_result])
                  OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_EXTRA_LIBS], [$opal_pkg_config_result])
                ]
          )
          OPAL_VAR_SCOPE_POP
        ],
        []
  )
])

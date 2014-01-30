dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013      Intel, Inc.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


AC_DEFUN([ORCM_CONFIGURE_OPTIONS],[
orcm_show_subtitle "ORCM Configuration options"

#
# Disable ORCM?
#
AC_ARG_ENABLE([orcm],
  [AC_HELP_STRING([--disable-orcm],
     [Disable building the ORCM code])])


])dnl

AC_DEFUN([ORCM_LOAD_CONFIGURATION],[
    if test "$with_platform" != "" ; then
        AC_MSG_CHECKING([for config file])
        # define an ORCM site configuration filename
        platform_site_config_file="`basename $platform_loaded`.xml"
        # look where platform file is located for platform.xml name
        if test -r "${platform_file_dir}/${platform_site_config_file}" ; then
            AC_SUBST(OPAL_SITE_CONFIG_FILE, [$platform_file_dir/$platform_site_config_file])
            AC_MSG_RESULT([$platform_file_dir/$platform_site_config_file])
            AC_SUBST(OPAL_SITE_CONFIG_FILE_FOUND, "yes")
        else
            AC_MSG_RESULT([no])
            AC_SUBST(OPAL_SITE_CONFIG_FILE_FOUND, "no")
        fi
    fi
])dnl

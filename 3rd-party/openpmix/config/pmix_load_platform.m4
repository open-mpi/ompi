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
dnl Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# PMIX_LOAD_PLATFORM()
# --------------------
AC_DEFUN([PMIX_LOAD_PLATFORM], [
    AC_ARG_WITH([pmix-platform-patches-dir],
        [AS_HELP_STRING([--with-pmix-platform-patches-dir=DIR],
                        [Location of the platform patches directory. If you use this option, you must also use --with-platform.])])

    AC_ARG_WITH([pmix-platform],
        [AS_HELP_STRING([--with-pmix-platform=FILE],
                        [Load options for build from FILE.  Options on the
                         command line not in FILE are used.  Options on the
                         command line and in FILE are replaced by what is
                         in FILE.])])
    m4_ifval([autogen_platform_file], [
        if test "$with_pmix_platform" = "" ; then
            with_pmix_platform=autogen_platform_file
        fi])
    if test "$with_pmix_platform" = "yes" ; then
        AC_MSG_ERROR([--with-pmix_platform argument must include FILE option])
    elif test "$with_pmix_platform" = "no" ; then
        AC_MSG_ERROR([--without-pmix_platform is not a valid argument])
    elif test "$with_pmix_platform" != "" ; then
        # if not an absolute path, check in contrib/platform
        if test ! "`echo $with_pmix_platform | cut -c1`" = "/" && test ! "`echo $with_pmix_platform | cut -c2`" = ".." ; then
            if test -r "${srcdir}/contrib/platform/$with_pmix_platform" ; then
                with_pmix_platform="${srcdir}/contrib/platform/$with_pmix_platform"
            fi
        fi

        # make sure file exists
        if test ! -r "$with_pmix_platform" ; then
            AC_MSG_ERROR([platform file $with_pmix_platform not found])
        fi

        # eval into environment
        PMIX_LOG_MSG([Loading environment file $with_pmix_platform, with contents below])
        PMIX_LOG_FILE([$with_pmix_platform])

        # setup by getting full pathname for the platform directories
        platform_base="`dirname $with_pmix_platform`"
        platform_file="`basename $with_pmix_platform`"
        # get full pathname of where we are so we can return
        platform_savedir="`pwd`"
        # go to where the platform file is located
        cd "$platform_base"
        # get the full path to this location
        platform_file_dir=`pwd`

        . ./"$platform_file"

        # see if they left us a name
        if test "$PMIX_PLATFORM_LOADED" != "" ; then
           platform_loaded="$PMIX_PLATFORM_LOADED"
        else
           platform_loaded="$with_pmix_platform"
        fi
        echo "Loaded platform arguments for $platform_loaded"
        PMIX_LOG_MSG([Loaded platform arguments for $platform_loaded])

        # look for default mca param file

        # return to where we started
        cd "$platform_savedir"

        # define an alternate default mca param filename
        platform_alt_mca_file="`basename $platform_loaded`.conf"

        # look where platform file is located for platform.conf name
        if test -r "${platform_file_dir}/${platform_alt_mca_file}" ; then
            AC_SUBST(PMIX_DEFAULT_MCA_PARAM_CONF, [$platform_file_dir/$platform_alt_mca_file])
            AC_SUBST(PMIX_PARAM_FROM_PLATFORM, "yes")
        # if not, see if a file is there with the default name
        elif test -r "${platform_file_dir}/pmix-mca-params.conf" ; then
            AC_SUBST(PMIX_DEFAULT_MCA_PARAM_CONF, [$platform_file_dir/pmix-mca-params.conf])
            AC_SUBST(PMIX_PARAM_FROM_PLATFORM, "yes")
        # if not, then just use the default
        else
            AC_SUBST(PMIX_DEFAULT_MCA_PARAM_CONF, [pmix-mca-params.conf])
            AC_SUBST(PMIX_PARAM_FROM_PLATFORM, "no")
        fi

        patch_dir="${with_pmix_platform}.patches"
        if test -n "$with_pmix_platform_patches_dir"; then
            if test "$with_pmix_platform_patches_dir" = "yes"; then
                patch_dir="${with_pmix_platform}.patches"
            elif test "$with_pmix_platform_patches_dir" = "no"; then
                AC_MSG_NOTICE([Disabling platform patches on user request])
                patch_dir=""
            elif test -d "$with_pmix_platform_patches_dir"; then
                patch_dir=$with_pmix_platform_patches_dir
            else
                AC_MSG_ERROR([User provided patches directory: $with_pmix_platform_patches_dir not found])
            fi
        fi

        patch_done="${srcdir}/.platform_patches"
        patch_found=no

        if test -d "${patch_dir}"; then
            if test ! -f "${patch_done}"; then

                AC_MSG_NOTICE([Checking patches from ${patch_dir}/ directory ])
                for one_patch in $patch_dir/*.patch ; do

                    AC_MSG_CHECKING([patch: $one_patch for errors ])
                    patch -d ${srcdir} -p1 -t -s --dry-run < ${one_patch}
                    if test "$?" != "0"; then
                        AC_MSG_RESULT([fail])
                        AC_MSG_ERROR([Platform patches failed to apply])
                    else
                        AC_MSG_RESULT([ok])
                    fi

                    AC_MSG_CHECKING([patch: $one_patch for unsupported configury changes ])
                    has_configury_items=$(patch -d ${srcdir} -p1 -t --dry-run < ${one_patch} 2>&1 | egrep "^patching" | egrep  '*\.(am|m4)$' | wc -l)

                    if test $has_configury_items -ne 0; then
                        AC_MSG_RESULT([fail])
                        AC_MSG_ERROR([Platform patches should not change configury files])
                    else
                        AC_MSG_RESULT([ok])
                    fi
                done


                for one_patch in $patch_dir/*.patch ; do
                    AC_MSG_NOTICE([Applying patch ${one_patch}])
                    patch -d ${srcdir} -p1 -t -s < ${one_patch}
                    if test "$?" != "0"; then
                        AC_MSG_ERROR([Failed to apply patch ${one_patch}])
                    fi
                    patch_found=yes
                done

                if test "$patch_found" = "yes"; then

                    platform_root_short="$(basename $platform_base)"

                    # If platform file resides under platform/ root folder - use filename as ident
                    if [ test "$platform_root_short" = "platform" ]; then
                        platform_ident="$platform_file"
                    else
                        platform_ident="$(basename $platform_base)"
                    fi

                    # Set custom ident for platform patched PMIX
                    if [ test -z "$with_ident_string" ]; then
                        with_ident_string="Platform: $platform_ident"
                    fi

                    AC_MSG_NOTICE([Platform patches applied, created stamp file ${patch_done}])
                    touch ${patch_done}
                else
                    AC_MSG_NOTICE([No platform patches in ${patch_dir}])
                fi

            else
                AC_MSG_WARN([Platform patches already applied, skipping. ${patch_done} can be removed to re-apply ])
            fi
        elif test -n "${patch_dir}"; then
          AC_MSG_NOTICE([No platform patches in ${patch_dir}])
        fi
    else
        AC_SUBST(PMIX_DEFAULT_MCA_PARAM_CONF, [pmix-mca-params.conf])
    fi
])

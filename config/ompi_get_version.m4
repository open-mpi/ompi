dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl
dnl This file is almost identical in functionality to
dnl ompi_get_version.sh.  It is unfortunate that we have to duplicate code,
dnl but it is really the only what that I can think to do it.  :-( Hence,
dnl if you change the logic here for determining version numbers, YOU MUST
dnl ALSO CHANGE IT IN ompi_get_version.sh!!
dnl 

AC_DEFUN([OMPI_GET_VERSION],[
gv_ver_file="$1"
gv_prefix="$2"

dnl quote eval to suppress macro expansion with non-GNU m4

gv_run() {
    str="${gv_prefix}_${2}=\$gv_${1}_version"
   [eval] $str
}

if test -n "$gv_ver_file" -a -f "$gv_ver_file"; then
    gv_major_version="`cat $gv_ver_file | grep major | cut -d= -f2`"
    gv_minor_version="`cat $gv_ver_file | grep minor | cut -d= -f2`"
    gv_release_version="`cat $gv_ver_file | grep release | cut -d= -f2`"
    gv_alpha_version="`cat $gv_ver_file | grep alpha | cut -d= -f2`"
    gv_beta_version="`cat $gv_ver_file | grep beta | cut -d= -f2`"
    gv_svn_version="`cat $gv_ver_file | grep svn | cut -d= -f2`"

    if test -n "$gv_release_version" -a "$gv_release_version" != "0"; then
	gv_full_version="$gv_major_version.$gv_minor_version.$gv_release_version"
    else
	gv_full_version="$gv_major_version.$gv_minor_version"
    fi

    if test "`expr $gv_alpha_version \> 0`" = "1"; then
	gv_full_version="${gv_full_version}a$gv_alpha_version"
    elif test "`expr $gv_beta_version \> 0`" = "1"; then
	gv_full_version="${gv_full_version}b$gv_beta_version"
    fi

    if test "$gv_svn_version" != "0"; then
        if test -d .svn; then
            ver="r`svnversion .`"
        else
            ver="svn`date '+%m%d%Y'`"
        fi
        gv_svn_version="$ver"
	gv_full_version="${gv_full_version}$ver"
    fi

    # Set the values

    gv_run full    VERSION
    gv_run major   MAJOR_VERSION
    gv_run minor   MINOR_VERSION
    gv_run release RELEASE_VERSION
    gv_run alpha   ALPHA_VERSION
    gv_run beta    BETA_VERSION
    gv_run svn     SVN_VERSION
fi

# Clean up

unset gv_glv_dir gv_ver_file gv_prefix gv_prog gv_run \
    gv_major_version gv_minor_version gv_release_version \
    gv_alpha_version gv_beta_version gv_svn_version
])

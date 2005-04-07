dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
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
    str="${gv_prefix}_${2}=\$gv_${1}"
   [eval] $str
}

if test -n "$gv_ver_file" -a -f "$gv_ver_file"; then
    gv_major_version="`egrep '^major=' $gv_ver_file | cut -d= -f2`"
    gv_minor_version="`egrep '^minor=' $gv_ver_file | cut -d= -f2`"
    gv_release_version="`egrep '^release=' $gv_ver_file | cut -d= -f2`"
    gv_alpha_version="`egrep '^alpha=' $gv_ver_file | cut -d= -f2`"
    gv_beta_version="`egrep '^beta=' $gv_ver_file | cut -d= -f2`"
    gv_want_svn="`egrep '^want_svn=' $gv_ver_file | cut -d= -f2`"
    gv_svn_r="`egrep '^svn_r=' $gv_ver_file | cut -d= -f2`"

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

    if test "$gv_want_svn" = "1"; then
        if test "$gv_svn_r" = "-1"; then
            if test -d "$srcdir/.svn"; then
                ver="r`svnversion \"$srcdir\"`"
            else
                ver="svn`date '+%m%d%Y'`"
            fi
            gv_svn_r="$ver"
        fi
	gv_full_version="${gv_full_version}$gv_svn_r"
    fi

    # Set the values

    gv_run full_version    VERSION
    gv_run major_version   MAJOR_VERSION
    gv_run minor_version   MINOR_VERSION
    gv_run release_version RELEASE_VERSION
    gv_run alpha_version   ALPHA_VERSION
    gv_run beta_version    BETA_VERSION
    gv_run want_svn        WANT_SVN
    gv_run svn_r           SVN_R
fi

# Clean up

unset gv_glv_dir gv_ver_file gv_prefix gv_prog gv_run \
    gv_major_version gv_minor_version gv_release_version \
    gv_alpha_version gv_beta_version gv_want_svn gv_svn_r
])

dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
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
dnl This file is also used as input to ompi_get_version.sh.
dnl

# OMPI_GET_VERSION(version_file, variable_prefix)
# -----------------------------------------------
# parse version_file for version information, setting
# the following shell variables:
#  
#  prefix_VERSION
#  prefix_BASE_VERSION
#  prefix_MAJOR_VERSION
#  prefix_MINOR_VERSION
#  prefix_RELEASE_VERSION
#  prefix_GREEK_VERSION
#  prefix_WANT_SVN
#  prefix_SVN_R
m4_define([OMPI_GET_VERSION],[
    : ${ompi_ver_need_svn=1}
    : ${srcdir=.}
    : ${svnversion_result=-1}

    dnl quote eval to suppress macro expansion with non-GNU m4
    if test -f "$1"; then
        [eval] "`sed -n \"\
	t clear
	: clear
	s/^major/$2_MAJOR_VERSION/
	s/^minor/$2_MINOR_VERSION/
	s/^release/$2_RELEASE_VERSION/
	s/^greek/$2_GREEK_VERSION/
	s/^want_svn/$2_WANT_SVN/
	s/^svn_r/$2_SVN_R/
	t print
	b
	: print
	p\" < \"\$1\"`"

        # Only print release version if it isn't 0
        if test $$2_RELEASE_VERSION -ne 0 ; then
            $2_VERSION="$$2_MAJOR_VERSION.$$2_MINOR_VERSION.$$2_RELEASE_VERSION"
        else
            $2_VERSION="$$2_MAJOR_VERSION.$$2_MINOR_VERSION"
        fi
        $2_VERSION="${$2_VERSION}${$2_GREEK_VERSION}"
        $2_BASE_VERSION=$$2_VERSION

        if test $$2_WANT_SVN -eq 1 && test $ompi_ver_need_svn -eq 1 ; then
            if test "$svnversion_result" != "-1" ; then
                $2_SVN_R=$svnversion_result
            fi
            if test "$$2_SVN_R" = "-1" ; then
                m4_ifdef([AC_MSG_CHECKING],
                         [AC_MSG_CHECKING([for SVN version])])
                if test -d "$srcdir/.svn" ; then
                    $2_SVN_R=r`svnversion "$srcdir"`
                    # make sure svnversion worked
                    if test $? -ne 0 ; then
                        $2_SVN_R=svn`date '+%m%d%Y'` 
                    fi
                    svnversion_result="$$2_SVN_R"
                else
                    $2_SVN_R=svn`date '+%m%d%Y'`
                fi
                m4_ifdef([AC_MSG_RESULT],
                         [AC_MSG_RESULT([done])])
            fi
            $2_VERSION="${$2_VERSION}${$2_SVN_R}"
        fi
    fi
])

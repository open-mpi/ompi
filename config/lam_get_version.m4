dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2003 The Trustees of Indiana University.  
dnl                    All rights reserved.
dnl 
dnl This file is part of the LAM software package.  For license
dnl information, see the LICENSE file in the top level directory of the
dnl LAM source distribution.
dnl
dnl $Id: lam_get_version.m4,v 1.1 2003/12/22 16:29:11 twoodall Exp $
dnl

define(LAM_GET_VERSION,[
gv_glv_dir="$1"
gv_ver_file="$2"
gv_prefix="$3"

# Find the get_lam_version program

gv_prog="sh $gv_glv_dir/lam_get_version.sh $gv_ver_file"

dnl quote eval to suppress macro expansion with non-GNU m4

gv_run() {
  [eval] ${gv_prefix}_${2}=`$gv_prog --${1}`
}

gv_run full    VERSION
gv_run major   MAJOR_VERSION
gv_run minor   MINOR_VERSION
gv_run release RELEASE_VERSION
gv_run alpha   ALPHA_VERSION
gv_run beta    BETA_VERSION
gv_run cvs     CVS_VERSION

# Clean up

unset gv_glv_dir gv_ver_file gv_prefix gv_prog gv_run
])

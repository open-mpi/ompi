dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_GET_VERSION],[
gv_glv_dir="$1"
gv_ver_file="$2"
gv_prefix="$3"

# Find the get_ompi_version program

gv_prog="sh $gv_glv_dir/ompi_get_version.sh $gv_ver_file"

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
gv_run svn     SVN_VERSION

# Clean up

unset gv_glv_dir gv_ver_file gv_prefix gv_prog gv_run
])

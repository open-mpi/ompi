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

AC_DEFUN([OMPI_CONFIG_SUBDIR_ARGS],[
#
# Invoke configure in subdirectories.
#
# $1 is the name of the variable to assign the output to
#

#
# Make a list of command line args --eliminate the --srcdir and
# --cache-file args, because we need to replace them with our own
# values when invoking the sub-configure script.
#

subdirs_args=
subdirs_skip=no

for subdirs_arg in $ac_configure_args; do
    if test "$subdirs_skip" = "yes"; then
	subdirs_skip=no
    else
	case $subdirs_arg in
	-cache-file | --cache-file | -cache | --cache)
	    subdirs_skip=yes
	    ;;
	--config-cache | -C)
	    ;;
	-cache-file=* | --cache-file=*)
	    ;;
	-srcdir | --srcdir)
	    subdirs_skip=yes
	    ;;
	-srcdir=* | --srcdir=*)
	    ;;
	*) 
	    subdirs_args="$subdirs_args $subdirs_arg" 
	    ;;
	esac
    fi
done

#
# Assign the output
#

subdirs_str="$1="'"'"$subdirs_args"'"'
eval $subdirs_str

#
# Clean up
#

unset subdirs_str subdirs_skip subdirs_args subdirs_arg])dnl

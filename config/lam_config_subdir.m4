dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([LAM_CONFIG_SUBDIR],[
#
# Invoke configure in a specific subdirectory.
#
# $1 is the directory to invoke in
# $2 is the list of arguments to pass
# $3 is actions to execute upon success
# $4 is actions to execute upon failure
#
subdir_dir="$1"
subdir_args="$2"
subdir_success="$3"
subdir_failure="$4"

#
# Sanity checks
#

if test "$subdir_dir" != ":" -a -d $srcdir/$subdir_dir; then
    AC_MSG_NOTICE([LAM configuring in $subdir_dir])

    #
    # Gotta check where srcdir is for VPATH builds.  If srcdir is not
    # ., then we need to mkdir the subdir.  Otherwise, we can just cd
    # into it.
    #

    case $srcdir in
    .) 
	;;
    *) 
	{ case $subdir_dir in
	[[\\/]]* | ?:[[\\/]]* ) total_dir=;;
	*)                      total_dir=.;;
	esac
	temp=$subdir_dir
	for dir_part in `IFS='/\\'; set X $temp; shift; echo "$[@]"`; do
	    case $dir_part in
	    # Skip DOS drivespec
	    ?:) total_dir=$dir_part ;;
	    *)  total_dir=$total_dir/$dir_part
		test -d "$total_dir" ||
		mkdir "$total_dir" ||
		AC_MSG_ERROR([cannot create $subdir_dir])
		;;
	    esac
	done; }

	if test -d ./$subdir_dir; then :;
	else
	    AC_MSG_ERROR([cannot create `pwd`/$subdir_dir])
	fi
	;;
    esac

    #
    # Move into the target directory
    #

    subdir_parent=`pwd`
    cd $subdir_dir

    #
    # Make a "../" for each directory in $subdir_dir.
    #

    subdir_dots=`[echo $subdir_dir | sed 's,^\./,,;s,[^/]$,&/,;s,[^/]*/,../,g]'`
    #
    # Construct the --srcdir argument
    #

    case $srcdir in
    .)
	# In place
	subdir_srcdir="$srcdir"
	;;
    [[\\/]* | ?:[\\/]*] )
	# Absolute path
	subdir_srcdir="$srcdir/$subdir_dir"
	;;
    *)
	# Relative path
	subdir_srcdir="$subdir_dots$srcdir/$subdir_dir"
	;;
    esac

    #
    # Construct the --cache-file argument
    #

    case $cache_file in
    [[\\/]* | ?:[\\/]*] )
	# Absolute path
	subdir_cache_file="$cache_file"
	;;
    *)
	# Relative path
        subdir_cache_file="$subdir_dots$cache_file"
	;;
    esac

    #
    # Invoke the configure script in the subdirectory
    #

    export CFLAGS CPPFLAGS
    export CXXFLAGS CXXCPPFLAGS
    export FFLAGS
    export LDFLAGS LIBS
    sub_configure="$SHELL '$subdir_srcdir/configure'"
    AC_MSG_NOTICE([running $sub_configure $subdir_args --cache-file=$subdir_cache_file --srcdir=$subdir_srcdir])
    eval $sub_configure $subdir_args \
	--cache-file=$subdir_cache_file --srcdir=$subdir_srcdir
    if test "$?" = "0"; then
	eval $subdir_success
	AC_MSG_NOTICE([$sub_configure succeeded for $subdir_dir])
    else
	eval $subdir_failure
	AC_MSG_NOTICE([$sub_configure *failed* for $subdir_dir])
    fi

    #
    # Go back to the topdir
    #

    cd $subdir_parent
fi

#
# Clean up
#

unset subdir_parent sub_configure subdir_dir subdir_srcdir subdir_cache_file
unset subdir_args subdir_dots total_dir dir_part temp])dnl

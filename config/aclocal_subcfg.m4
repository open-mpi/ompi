dnl Sandbox configure with additional arguments
dnl Usage: PAC_CONFIG_SUBDIR_ARGS(subdir,configure-args,configure-args-to-remove,action-if-success,action-if-failure)
dnl
dnl The subconfigure argument list is created based on "ac_precious_vars"
dnl instead of explicitly use of well-known Makefile variables, like
dnl CC/CFLAGS/CPPFLAGS..., this generalization is effective as long as
dnl calling configure.ac declares the needed variables to be passed down
dnl to subconfigure as "precious" appropriately.  The precious variable
dnl can be created in the following ways:
dnl 1) implicit declaration through use of autoconf macros, like
dnl    AC_PROG_CC (declares CC/CFLAGS/CPPFLAGS/LIBS/LDFLAGS), or
dnl    AC_PROG_F77 (declares F77/FFLAGS/FLIBS) ... 
dnl    which are in turns invoked by other subconfigure.
dnl    When in doubt, check "ac_precious_var" in the calling configure.
dnl 2) explicit "precious" declaration through AC_ARG_VAR.
dnl Without correct "precious" declaration in the calling configure.ac,
dnl there would be variables not being included in the subconfigure
dnl argument list.
dnl
dnl Note: I suspect this DEFUN body is underquoted in places, but it does not
dnl seem to cause problems in practice yet. [goodell@ 2010-05-18]
AC_DEFUN([PAC_CONFIG_SUBDIR_ARGS],[
        dnl BWB: hack to make --help=recursive work with these
        dnl configure options.  Lifted from AC_CONFIG_SUBDIRS.
        m4_append([_AC_LIST_SUBDIRS], [$1], [
])dnl

        pac_dir="$1"
        AC_MSG_NOTICE([===== configuring $1 =====])

	pac_abs_srcdir=`(cd $srcdir && pwd)`

	if test -f $pac_abs_srcdir/$1/setup ; then
           AC_MSG_NOTICE([sourcing $pac_abs_srcdir/$1/setup])
	   . $pac_abs_srcdir/$1/setup
	fi

        # Adapted for MPICH from the autoconf-2.67 implementation of
        # AC_CONFIG_SUBDIRS.  Search for "MPICH note:" for relevant commentary and
        # local modifications.

        # Remove --cache-file, --srcdir, and --disable-option-checking arguments
        # so they do not pile up.  Otherwise relative paths (like --srcdir=.. from
        # make distcheck) will be incorrect.
        pac_sub_configure_args="$2"
        pac_prev=
        eval "set x $ac_configure_args"
        shift
        for pac_arg
        do
          if test -n "$pac_prev"; then
            pac_prev=
            continue
          fi
          case $pac_arg in
          -cache-file | --cache-file | --cache-fil | --cache-fi \
          | --cache-f | --cache- | --cache | --cach | --cac | --ca | --c)
            pac_prev=cache_file ;;
          -cache-file=* | --cache-file=* | --cache-fil=* | --cache-fi=* \
          | --cache-f=* | --cache-=* | --cache=* | --cach=* | --cac=* | --ca=* \
          | --c=*)
            ;;
          --config-cache | -C)
            ;;
          -srcdir | --srcdir | --srcdi | --srcd | --src | --sr)
            pac_prev=srcdir ;;
          -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=*)
            ;;
          -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
            pac_prev=prefix ;;
          -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
            ;;
          --disable-option-checking)
            ;;
          *)
            # strip out precious variables from ac_configure_args,
            # which will include precious variables that are currently
            # set and were set on the command line or in the
            # environment at the time configure was invoked.  Instead,
            # we add all precious variables which have been tagged as
            # set, so that we can more closely control the environment
            # of sub-configures.
            is_precious=0
            for pac_pvar in $ac_precious_vars ; do
                # check if configure argument token contains the
                # precious variable, i.e. "name_of_prec_var=".
                if ( echo $pac_arg | grep "^$pac_pvar=" >/dev/null 2>&1 ) ; then
                    is_precious=1
                    break
                fi
            done
            if test $is_precious -eq 0; then
              case $pac_arg in
              *\'*) pac_arg=`AS_ECHO(["$pac_arg"]) | sed "s/'/'\\\\\\\\''/g"` ;;
              esac
              AS_VAR_APPEND([pac_sub_configure_args], [" '$pac_arg'"]) 
            fi ;;
          esac
        done

        # add all precious values with a set token to the configure
        # args.  If the caller hasn't artificially manipulated the
        # environment, this will simply be any precious variables as
        # they were originally specified on the top-level configure
        # line (or in the environment at start of configure).
        # However, callers may manipulate that environment, preferably
        # with the OPAL_SUBDIR_ENV macros.
        for temp_var in $ac_precious_vars; do
            eval temp_var_set=\$ac_env_${temp_var}_set
            if test "$temp_var_set" = "set" ; then
                eval temp_val=\$$temp_var
                temp_arg="$temp_var=$temp_val"
                AS_VAR_APPEND([pac_sub_configure_args], [" '$temp_arg'"])
            fi
        done

        # Always prepend --prefix to ensure using the same prefix
        # in subdir configurations.
        # MPICH note: see tt#983 for an example of why this is necessary
        pac_arg="--prefix=$prefix"
        case $pac_arg in
        *\'*) pac_arg=`AS_ECHO(["$pac_arg"]) | sed "s/'/'\\\\\\\\''/g"` ;;
        esac
        pac_sub_configure_args="'$pac_arg' $pac_sub_configure_args"

        # Pass --silent
        if test "$silent" = yes; then
          pac_sub_configure_args="--silent $pac_sub_configure_args"
        fi

        # Always prepend --disable-option-checking to silence warnings, since
        # different subdirs can have different --enable and --with options.
        pac_sub_configure_args="--disable-option-checking $pac_sub_configure_args"

        # remove arguments specified in third argument from the
        # built-up list of arguments
        m4_ifnblank([$3],
                    [m4_foreach(opt, [$3], [pac_sub_configure_args=$(echo $pac_sub_configure_args | sed "s,'opt',,")
         ])])

        pac_popdir=`pwd`

        AS_MKDIR_P(["$pac_dir"])
        # MPICH note: we leave this internal macro reference for now.  We can clone
        # the macro locally if this turns out to be non-portable across several autoconf
        # versions.  It sets the following variables: ac_builddir,
        # ac_top_builddir_sub, ac_top_build_prefix, ac_srcdir, ac_top_srcdir,
        # ac_abs_top_builddir, ac_abs_builddir, ac_abs_top_srcdir, ac_abs_srcdir
        _AC_SRCDIRS(["$pac_dir"])

        cd "$pac_dir"

        # Check for guested configure; otherwise get Cygnus style
        # configure.  Look for configure in source tree and then the
        # build tree, as we sometimes configure from sub-tarballs
        # expanded in the build tree.
        if test -f "$ac_srcdir/configure.gnu"; then
          pac_sub_configure=$ac_srcdir/configure.gnu
        elif test -f "$ac_srcdir/configure"; then
          pac_sub_configure=$ac_srcdir/configure
        elif test -f "configure.gnu"; then
	  pac_sub_configure="configure.gnu"
          ac_srcdir="."
        elif test -f "configure"; then
	  pac_sub_configure="configure"
          ac_srcdir="."
        else
          AC_MSG_WARN([no configuration information is in $pac_dir])
          pac_sub_configure=
        fi

        # The recursion is here.
        if test -n "$pac_sub_configure"; then
          # MPICH note: overriding the cache file on purpose to prevent strange
          # issues resulting from inter-dir caching
          pac_sub_cache_file="/dev/null"

          AC_MSG_NOTICE([running $SHELL $pac_sub_configure $pac_sub_configure_args --cache-file=$pac_sub_cache_file --srcdir=$ac_srcdir])
          # The eval makes quoting arguments work.
          # MPICH note: we want to execute the provided actions, not be silent
          # or error out if the subconfigure succeeded/failed
          if eval "\$SHELL \"\$pac_sub_configure\" $pac_sub_configure_args \
               --cache-file=\"\$pac_sub_cache_file\" --srcdir=\"\$ac_srcdir\""
          then
            # restore the current dir for the provided actions
            cd "$pac_popdir"
            $4
          else
            # restore the current dir for the provided actions
            cd "$pac_popdir"
            $5
          fi
        fi

        cd "$pac_popdir"

        AC_MSG_NOTICE([===== done with $1 configure =====])
])

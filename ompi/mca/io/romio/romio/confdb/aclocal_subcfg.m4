dnl PAC_RESET_ALL_FLAGS - Reset precious flags to those set by the user
AC_DEFUN([PAC_RESET_ALL_FLAGS],[
	if test "$FROM_MPICH" = "yes" ; then
	   CFLAGS="$USER_CFLAGS"
	   CPPFLAGS="$USER_CPPFLAGS"
	   CXXFLAGS="$USER_CXXFLAGS"
	   FFLAGS="$USER_FFLAGS"
	   FCFLAGS="$USER_FCFLAGS"
	   LDFLAGS="$USER_LDFLAGS"
	   LIBS="$USER_LIBS"
	fi
])

dnl PAC_RESET_LINK_FLAGS - Reset precious link flags to those set by the user
AC_DEFUN([PAC_RESET_LINK_FLAGS],[
	if test "$FROM_MPICH" = "yes" ; then
	   LDFLAGS="$USER_LDFLAGS"
	   LIBS="$USER_LIBS"
	fi
])

dnl Sandbox configure with additional arguments
dnl Usage: PAC_CONFIG_SUBDIR_ARGS(subdir,configure-args,action-if-success,action-if-failure)
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
            # MPICH note: this is a more robust version of the "precious
            # variable" propagation code that was present in the previous
            # incarnation of this macro
            for pac_pvar in $ac_precious_vars ; do
                # check if configure argument token contains the
                # precious variable, i.e. "name_of_prec_var=".
                if ( echo $pac_arg | grep "^$pac_pvar=" >/dev/null 2>&1 ) ; then
                    # check if current precious variable is set in env
                    eval pvar_set=\${$pac_pvar+set}
                    if test "$pvar_set" = "set" ; then
                        # Append 'name_of_prec_var=value_of_prec_var'
                        # to the subconfigure arguments list, where
                        # value_of_prec_var is fetched from the env.
                        # this also overrides any value set on the command line
                        eval pac_pval=\${$pac_pvar}
                        pac_arg="$pac_pvar=$pac_pval"
                        break
                    fi
                fi
            done
            case $pac_arg in
            *\'*) pac_arg=`AS_ECHO(["$pac_arg"]) | sed "s/'/'\\\\\\\\''/g"` ;;
            esac
            AS_VAR_APPEND([pac_sub_configure_args], [" '$pac_arg'"]) ;;
          esac
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
      
        pac_popdir=`pwd`
      
        # Do not complain, so a configure script can configure whichever
        # parts of a large source tree are present.
        test -d "$srcdir/$pac_dir" || continue
      
        # MPICH note: modified to remove the internal "_AS_*" macro usage, also
        # msg is already printed at top
dnl        _AS_ECHO_LOG([$pac_msg])
dnl        _AS_ECHO([$pac_msg])
        AS_MKDIR_P(["$pac_dir"])
        # MPICH note: we leave this internal macro reference for now.  We can clone
        # the macro locally if this turns out to be non-portable across several autoconf
        # versions.  It sets the following variables: ac_builddir,
        # ac_top_builddir_sub, ac_top_build_prefix, ac_srcdir, ac_top_srcdir,
        # ac_abs_top_builddir, ac_abs_builddir, ac_abs_top_srcdir, ac_abs_srcdir
        _AC_SRCDIRS(["$pac_dir"])

        cd "$pac_dir"

        # Check for guested configure; otherwise get Cygnus style configure.
        if test -f "$ac_srcdir/configure.gnu"; then
          pac_sub_configure=$ac_srcdir/configure.gnu
        elif test -f "$ac_srcdir/configure"; then
          pac_sub_configure=$ac_srcdir/configure
        elif test -f "$ac_srcdir/configure.ac"; then
          # This should be Cygnus configure.
          pac_sub_configure=$ac_aux_dir/configure
        else
          AC_MSG_WARN([no configuration information is in $pac_dir])
          pac_sub_configure=
        fi

        # The recursion is here.
        if test -n "$pac_sub_configure"; then
          # MPICH note: overriding the cache file on purpose to prevent strange
          # issues resulting from inter-dir caching
dnl          # Make the cache file name correct relative to the subdirectory.
dnl          case $cache_file in
dnl          [[\\/]]* | ?:[[\\/]]* ) pac_sub_cache_file=$cache_file ;;
dnl          *) # Relative name.
dnl            pac_sub_cache_file=$ac_top_build_prefix$cache_file ;;
dnl          esac
          pac_sub_cache_file="/dev/null"

          AC_MSG_NOTICE([running $SHELL $pac_sub_configure $pac_sub_configure_args --cache-file=$pac_sub_cache_file --srcdir=$ac_srcdir])
          # The eval makes quoting arguments work.
          # MPICH note: we want to execute the provided actions, not be silent
          # or error out if the subconfigure succeeded/failed
dnl          eval "\$SHELL \"\$pac_sub_configure\" $pac_sub_configure_args \
dnl               --cache-file=\"\$pac_sub_cache_file\" --srcdir=\"\$ac_srcdir\"" ||
dnl            AC_MSG_ERROR([$pac_sub_configure failed for $pac_dir])
          if eval "\$SHELL \"\$pac_sub_configure\" $pac_sub_configure_args \
               --cache-file=\"\$pac_sub_cache_file\" --srcdir=\"\$ac_srcdir\""
          then
            # restore the current dir for the provided actions
            cd "$pac_popdir"
            $3
          else
            # restore the current dir for the provided actions
            cd "$pac_popdir"
            $4
          fi
        fi

        cd "$pac_popdir"

        AC_MSG_NOTICE([===== done with $1 configure =====])

	# Check for any localdefs files.  These may be created, so we
	# look in the local directory first.
	if test -f $1/localdefs ; then
           AC_MSG_NOTICE([sourcing $1/localdefs])
	   . $1/localdefs
	elif test -f $pac_abs_srcdir/$1/localdefs ; then
           AC_MSG_NOTICE([sourcing $pac_abs_srcdir/$1/localdefs])
	   . $pac_abs_srcdir/$1/localdefs
	fi
])

dnl Sandbox configure
dnl Usage: PAC_CONFIG_SUBDIR(subdir,action-if-success,action-if-failure)
AC_DEFUN([PAC_CONFIG_SUBDIR],[PAC_CONFIG_SUBDIR_ARGS([$1],[],[$2],[$3])])

dnl PAC_SUBCFG_EXPAND_SUFFIX_MACRO(MACRO_PREFIX,MACRO_PATH_SUFFIX)
dnl converts the path given by MACRO_PATH_SUFFIX (with '/' chars in it) to one
dnl with '_' chars in it and then appends that to MACRO_PREFIX with '_' in
dnl between.  The resulting macro name is then expanded, but with informative
dnl "##" comments before and after the expansion.
dnl
dnl This is intended to be an internal helper macro for the PAC_SUBCFG
dnl implementation.
dnl
dnl XXX DJG FIXME: need to be able to deal with PREREQ macros that potentially
dnl aren't present while having safety for BODY macros when there are
dnl misspellings
AC_DEFUN([PAC_SUBCFG_EXPAND_PATH_SUFFIX_MACRO],[
dnl convert path separators into '_', the m4_translit is intentionally unquoted
m4_pushdef([subsys_uscore_name],[$1_]m4_translit([$2],[\/],[__]))dnl
m4_ifdef(m4_defn([subsys_uscore_name]),[],[m4_fatal([macro ]m4_defn([subsys_uscore_name])[ is undefined])])dnl
[##] begin expansion of m4_defn([subsys_uscore_name])
dnl call the computed routine name with the remaining args
m4_indir(m4_defn([subsys_uscore_name]),m4_shift($@))
dnl there is intentionally no "dnl" on the previous line to reduce the chance of
dnl a "fi## end expansion" bug when the BODY macro doesn't end in a newline
[##] end expansion of m4_defn([subsys_uscore_name])
])

dnl invokes the PAC_SUBCFG_BODY_foo macro for the "foo" subsys, when "foo" is
dnl passed as the only argument to this macro.  The first arg may be the '/'
dnl path version instead of having underscores.
AC_DEFUN([PAC_SUBCFG_CONFIGURE_SUBSYS],[PAC_SUBCFG_EXPAND_PATH_SUFFIX_MACRO([PAC_SUBCFG_BODY],[$1])])

dnl invokes the PAC_SUBCFG_PREREQ_foo macro for the "foo" subsys, when "foo" is
dnl passed as the only argument to this macro.  The first arg may be the '/'
dnl path version instead of having underscores.
AC_DEFUN([PAC_SUBCFG_DO_PREREQ],[PAC_SUBCFG_EXPAND_PATH_SUFFIX_MACRO([PAC_SUBCFG_PREREQ],[$1])])

dnl takes no arguments, expands to "foo_bar_baz" when invoked in a file named
dnl "foo/bar/baz/subconfigure.m4"
dnl
dnl This is useful for reducing copy-paste errors when defining PREREQ and BODY
dnl macros.  If you tinker with this macro, watch the quoting carefully.
AC_DEFUN([PAC_SUBCFG_AUTO_SUFFIX],[m4_translit(m4_bpatsubst(m4_dquote(__file__),[/[^/]+.m4],[]),[/],[_])])


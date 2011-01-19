dnl PAC_RESET_ALL_FLAGS - Reset precious flags to those set by the user
AC_DEFUN([PAC_RESET_ALL_FLAGS],[
	if test "$FROM_MPICH2" = "yes" ; then
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
	if test "$FROM_MPICH2" = "yes" ; then
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
dnl calling configure.in declares the needed variables to be passed down
dnl to subconfigure as "precious" appropriately.  The precious variable
dnl can be created in the following ways:
dnl 1) implicit declaration through use of autoconf macros, like
dnl    AC_PROG_CC (declares CC/CFLAGS/CPPFLAGS/LIBS/LDFLAGS), or
dnl    AC_PROG_F77 (declares F77/FFLAGS/FLIBS) ... 
dnl    which are in turns invoked by other subconfigure.
dnl    When in doubt, check "ac_precious_var" in the calling configure.
dnl 2) explicit "precious" declaration through AC_ARG_VAR.
dnl Without correct "precious" declaration in the calling configure.in,
dnl there would be variables not being included in the subconfigure
dnl argument list.
dnl
dnl Note: I suspect this DEFUN body is underquoted in places, but it does not
dnl seem to cause problems in practice yet. [goodell@ 2010-05-18]
AC_DEFUN([PAC_CONFIG_SUBDIR_ARGS],[
        AC_MSG_NOTICE([===== configuring $1 =====])

	PAC_MKDIRS($1)
	pac_abs_srcdir=`(cd $srcdir && pwd)`

	if test -f $pac_abs_srcdir/$1/setup ; then
	   . $pac_abs_srcdir/$1/setup
	fi

        pac_subconfigure_file="$pac_abs_srcdir/$1/configure"
	if test -x $pac_subconfigure_file ; then
	   pac_subconfig_args="$2"

            # Set IFS so ac_configure_args can be tokenized
            # with extra " " tokens being skipped.
            saved_IFS="$IFS"
            IFS="'"
            for pac_arg in $ac_configure_args ; do
                case "$pac_arg" in
                # Ignore any null and leading blank strings.
                ""|" "*)
                    ;;
                *)
                    pac_pval=""
                    # Restore saved IFS so ac_precious_vars which has
                    # " " as separator can be correctly tokenized
                    IFS="$saved_IFS"
                    for pac_pvar in $ac_precious_vars ; do
                        # check if configure argument token contains the
                        # precious variable, i.e. "name_of_prec_var=".
                        pvar_in_arg=`echo $pac_arg | grep "$pac_pvar="`
                        if test "X$pvar_in_arg" != "X" ; then
                            # check if current precious variable is set in env
                            eval pvar_set=\${$pac_pvar+set}
                            if test "$pvar_set" = "set" ; then
                                # Append 'name_of_prec_var=value_of_prec_var'
                                # to the subconfigure arguments list, where
                                # value_of_prec_var is fetched from the env.
                                eval pac_pval=\${$pac_pvar}
                                pac_subconfig_args="$pac_subconfig_args '$pac_pvar=$pac_pval'"
                                break
                            fi
                        fi
                    done
                    # since the precious variable is not set in the env.,
                    # append the corresponding configure argument token
                    # to the subconfigure argument list.
                    if test "X$pac_pval" = "X" ; then
                        pac_subconfig_args="$pac_subconfig_args '$pac_arg'"
                    fi
                    # reset "'" as IFS to process ac_configure_args
                    saved_IFS="$IFS"
                    IFS="'"
                    ;;
                esac
            done
            # Restore IFS.
            IFS="$saved_IFS"
            dnl echo "pac_subconfig_args = |$pac_subconfig_args|"

           dnl Add option to disable configure options checking
           if test "$enable_option_checking" = no ; then
              pac_subconfig_args="$pac_subconfig_args --disable-option-checking"
           fi

	   AC_MSG_NOTICE([executing: $pac_subconfigure_file $pac_subconfig_args])
	   if (cd $1 && eval $pac_subconfigure_file $pac_subconfig_args) ; then
               ifelse([$3],[],[:],[$3])
	   else
               ifelse([$4],[],[:],[$4])
	   fi
        else
           if test -e $pac_subconfigure_file ; then
               AC_MSG_WARN([$pac_subconfigure_file exists but is not executable])
           else
               AC_MSG_WARN([$pac_subconfigure_file does not exist])
           fi
	fi

        AC_MSG_NOTICE([===== done with $1 configure =====])

	# Check for any localdefs files.  These may be created, so we
	# look in the local directory first.
	if test -f $1/localdefs ; then
	   . $1/localdefs
	elif test -f $pac_abs_srcdir/$1/localdefs ; then
	   . $pac_abs_srcdir/$1/localdefs
	fi
])

dnl Sandbox configure
dnl Usage: PAC_CONFIG_SUBDIR(subdir,action-if-success,action-if-failure)
AC_DEFUN([PAC_CONFIG_SUBDIR],[PAC_CONFIG_SUBDIR_ARGS([$1],[],[$2],[$3])])


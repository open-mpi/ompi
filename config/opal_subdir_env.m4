dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl OPAL_SUBDIR_ENV_CLEAN([prefix])
dnl
dnl Save all precious variables with the specified prefix and reset
dnl all precious variables to their state at the start of configure.
dnl That is, if a precious variable (say CPPFLAGS) was set on the
dnl configure command line or in the environment at the start of
dnl configure, it will be reset to that stored value.  Otherwise, it
dnl will be unset.  The current state is recorded so that it can be
dnl restored with OPAL_SUBDIR_ENV_RESTORE.  Useful for wrapping calls
dnl to subconfigure scripts.
AC_DEFUN([OPAL_SUBDIR_ENV_CLEAN], [
    for temp_var in $ac_precious_vars; do
        # save all set variables, with a <prefix>_<name>_set and
        # <prefix>_<name>_value format.  _set will always be saved,
        # _value if _set evaluates to "set".
        #
        # Because we may modify the environment with
        # OPAL_SUBDIR_ENV_APPEND, also store the original values of
        # ac_env_<name>_set in <prefix>_ac_env_<name>_set
        eval temp_var_set=\${${temp_var}+set}
        eval $1_${temp_var}_set=$temp_var_set
        eval $1_ac_env_${temp_var}_set=\$ac_env_${temp_var}_set
        if test "$temp_var_set" = "set" ; then
            eval $1_${temp_var}_value=\$${temp_var}
        fi
        unset tmp_var_set

        # restore the variables that were set at the start of
        # configure and unset the ones that were not.
        eval temp_var_orig_set=\$ac_env_${temp_var}_set
        if test "$temp_var_set" = "set" ; then
             eval ${temp_var}=\$ac_env_${temp_var}_value
        else
             unset $temp_var
        fi
    done
])

dnl OPAL_SUBDIR_ENV_RESTORE([prefix])
dnl
dnl Match with call to OPAL_SUBDIR_ENV_CLEAN.  Restore will return all
dnl precious variables to their state at the time that
dnl OPAL_SUBDIR_ENV_CLEAN was called.  If a variable was set at that
dnl time, it will be set to that value.  Otherwise, it will be unset.
dnl
dnl The state of the ac_env_<var>_set value is also restored to its
dnl value from when OPAL_SUBDIR_ENV_CLEAN is called.  The
dnl ac_env_<var>_set variables may be manipulated by
dnl OPAL_SUBDIR_ENV_APPEND.
AC_DEFUN([OPAL_SUBDIR_ENV_RESTORE], [
    for temp_var in $ac_precious_vars; do
        # always restore the value of ac_env_<name>_set
        eval ac_env_${temp_var}_set=\$$1_ac_env_${temp_var}_set

        # conditionally restore any variable values that were set at
        # CLEAN time
        eval temp_var_set=\$$1_${temp_var}_set
        if test "$temp_var_set" = "set" ; then
            eval ${temp_var}=\$$1_${temp_var}_value
        fi

        unset $1_${temp_var}_value
        unset $1_${temp_var}_set
        unset $1_ac_env_${temp_var}_set
    done
])

dnl OPAL_SUBDIR_ENV_APPEND([precious variable], [append value])
dnl
dnl Append the contents of [append value] to precious variable
dnl [precious variable] and set the ac_env_<param>_set variable so
dnl that PAC_CONFIG_SUBDIR_ARGS can pick up the variables for
dnl subconfigure runs.  Most useful between a call to
dnl OPAL_SUBDIR_ENV_CLEAN and OPAL_SUBDIR_ENV_RESTORE
AC_DEFUN([OPAL_SUBDIR_ENV_APPEND], [
    ac_env_$1_set="set"
    AS_IF([test -z "$$1"], [$1=$2], [$1="$$1 $2"])
])

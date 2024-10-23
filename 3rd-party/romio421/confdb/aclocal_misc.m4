dnl PAC_CHECK_PYTHON check for python 3, sets PYTHON variable or abort
dnl
AC_DEFUN([PAC_CHECK_PYTHON],[
    AC_ARG_VAR([PYTHON], [set to Python 3])
    if test -z "$PYTHON" ; then
        AC_MSG_CHECKING([Python 3])
        PYTHON=
        python_one_liner="import sys; print(sys.version_info[[0]])"
        if test 3 = `python -c "$python_one_liner"`; then
            PYTHON=python
        elif test 3 = `python3 -c "$python_one_liner"`; then
            PYTHON=python3
        fi
        AC_MSG_RESULT($PYTHON)
        if test -z "$PYTHON" ; then
            AC_MSG_WARN([Python 3 not found! Bindings need to be generated before configure.])
        fi
    fi
])

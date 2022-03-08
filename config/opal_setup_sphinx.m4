dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2020-2022 Cisco Systems, Inc.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OPAL_SETUP_SPHINX],[
    OPAL_VAR_SCOPE_PUSH([summary_msg sphinx_result opal_install_docs sphinx_target_version sphinx_found_version])

    # This option is probably only helpful to developers: have
    # configure fail if Sphinx is not found (i.e., if you don't have
    # the ability to use Sphinx to build the HTML docs and man pages).
    AC_ARG_ENABLE([sphinx],
        [AS_HELP_STRING([--enable-sphinx],
            [Force configure to fail if Sphinx is not found (Sphinx is used to build the Open MPI and OpenSHMEM HTML docs and man pages).  This option is likely only useful for Open MPI developers; end users who are building from Open MPI distribution tarballs do ***not*** need to have Sphinx installed])])

    # Quick check to see if we have already-built docs (e.g., if we're
    # in a tarball vs. a fresh git clone).
    AC_MSG_CHECKING([if pre-built docs are available])
    AS_IF([test -f "$srcdir/docs/_build/man/MPI_T.5"],
          [opal_install_docs=1
           AC_MSG_RESULT([yes])],
          [opal_install_docs=0
           AC_MSG_RESULT([no])])

    # To generate HTML docs + man pages, we need Sphinx.  If we have
    # Sphinx, then we're able to both build and install the docs
    # (potentially overriding opal_install_docs from what it was set
    # above).
    AC_PATH_PROG([SPHINX_BUILD], [sphinx-build], [])

    # If the user requested to disable sphinx, then pretend we didn't
    # find it.
    AS_IF([test "$enable_sphinx" = "no"],
          [SPHINX_BUILD=])

    # If we found Sphinx, check to ensure that it's a recent enough
    # version.
    AS_IF([test -n "$SPHINX_BUILD"],
          [[sphinx_target_version=`sed -n -e 's/sphinx[><=]*\([0-9\.]\)/\1/p' $srcdir/docs/requirements.txt`]
           sphinx_found_version=`$SPHINX_BUILD --version 2>&1 | cut -d\  -f2`
           AC_MSG_CHECKING([if Sphinx version is high enough ($sphinx_found_version >= $sphinx_target_version)])
           AS_VERSION_COMPARE([$sphinx_found_version],
                              [$sphinx_target_version],
                              [sphinx_result=lesser],
                              [sphinx_result=equal],
                              [sphinx_result=greater])
           AS_IF([test "$sphinx_result" = "lesser"],
                 [SPHINX_BUILD=
                  AC_MSG_RESULT([no])],
                 [ # If we're building, we're also installing, regardless of
                   # whether we found pre-build docs or not (above).
                  opal_install_docs=1
                  AC_MSG_RESULT([yes])])
          ])

    AS_IF([test -z "$SPHINX_BUILD"],
          [OPAL_MAKEDIST_DISABLE="$OPAL_MAKEDIST_DISABLE Sphinx/Documentation"
           AC_MSG_NOTICE([Could not find a suitable sphinx-build on your system.])
           AC_MSG_NOTICE([You will not be able to build a distribution tarball.])
          ])

    AS_IF([test $opal_install_docs -eq 0],
          [AC_MSG_WARN([*** You will not have documentation installed.])
           AC_MSG_WARN([*** See the following URL for more information:])
           dnl Note that we have to double escape the string below
           dnl so that the # it contains coesn't confuse the Autotools
           AC_MSG_WARN([[***   https://ompi.readthedocs.io/en/latest/developers/prerequisites.html#sphinx]])
          ])

    # If --enable-sphinx was specified and we did not find Sphinx,
    # abort.  This is likely only useful to prevent "oops!" moments
    # from Open MPI developers.
    AS_IF([test -z "$SPHINX_BUILD" && test "$enable_sphinx" = "yes"],
          [AC_MSG_WARN([Sphinx was not found, but --enable-sphinx was specified])
           AC_MSG_ERROR([Cannot continue])])

    # Construct a summary message.  Due SUMMARY_ADD's implementation,
    # do *not* include a comma.
    AS_IF([test -n "$SPHINX_BUILD"],
          [ # If we found Sphinx, we always both build and install.
           summary_msg="building and installing"],
          [AS_IF([test $opal_install_docs -eq 1],
                 [summary_msg="installing packaged docs"],
                 [summary_msg="no documentation available"])])

    OPAL_SUMMARY_ADD([Miscellaneous], [HTML docs and man pages], [],
                     [$summary_msg])

    AM_CONDITIONAL([OPAL_BUILD_DOCS], [test -n "$SPHINX_BUILD"])
    AM_CONDITIONAL([OPAL_INSTALL_DOCS], [test $opal_install_docs -eq 1])
    OPAL_VAR_SCOPE_POP
])

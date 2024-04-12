dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2020-2022 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2024 Jeffrey M. Squyres.  All rights reserved.
dnl
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Setup Sphinx for building HTML docs and man pages
dnl
dnl 1 -> sanity file to check if pre-built docs are already available
dnl      You probably want to pass something like
dnl      "$srcdir/docs/_build/man/foo.1"
dnl
dnl 2 -> (OPTIONAL) URL to display in AC_MSG_WARN when docs will not be installed
dnl      If $2 is empty, nothing will be displayed.
dnl      Note: if $2 contains a #, be sure to double quote it
dnl      (e.g., [[https://example.com/foo.html#some-anchor]])
dnl
dnl 3 -> (OPTIONAL) Filename of requirements.txt-like file containing
dnl      the required Pip modules (to be displayed if rendering a
dnl      simple RST project fails).
dnl
dnl This macro requires that OAC_PUSH_PREFIX was previously called.
dnl The pushed prefix may be used if this macro chooses to set {OAC
dnl prefix}_MAKEDIST_DISABLE.  If set, it is a message indicating why
dnl "make dist" should be disabled, suitable for emitting via
dnl AC_MSG_WARN.
AC_DEFUN([OAC_SETUP_SPHINX],[
    OAC_ASSERT_PREFIX_DEFINED([$0])
    OAC_VAR_SCOPE_PUSH([oac_summary_msg oac_sphinx_result oac_install_docs oac_sphinx_target_version oac_sphinx_found_version])

    # This option is probably only helpful to developers: have
    # configure fail if Sphinx is not found (i.e., if you don't have
    # the ability to use Sphinx to build the HTML docs and man pages).
    AC_ARG_ENABLE([sphinx],
        [AS_HELP_STRING([--enable-sphinx],
            [Force configure to fail if Sphinx is not found (Sphinx is used to build the HTML docs and man pages).  This option is likely only useful for developers; end users who are building from distribution tarballs do ***not*** need to have Sphinx installed])])

    # Quick check to see if we have already-built docs (e.g., if we're
    # in a tarball vs. a fresh git clone).
    AC_MSG_CHECKING([if pre-built docs are available])
    AS_IF([test -f "$1"],
          [oac_install_docs=1
           AC_MSG_RESULT([yes])],
          [oac_install_docs=0
           AC_MSG_RESULT([no])])

    # To generate HTML docs + man pages, we need Sphinx.  If we have
    # Sphinx, then we're able to both build and install the docs
    # (potentially overriding oac_install_docs from what it was set
    # above).
    AC_PATH_PROG([SPHINX_BUILD], [sphinx-build], [])

    # If the user requested to disable sphinx, then pretend we didn't
    # find it.
    AS_IF([test "$enable_sphinx" = "no"],
          [SPHINX_BUILD=])

    # If we found Sphinx, check to ensure that it's a recent enough
    # version.
    AS_IF([test -n "$SPHINX_BUILD"],
          [[oac_sphinx_target_version=`sed -n -e 's/sphinx[><=]*\([0-9\.]\)/\1/p' $srcdir/docs/requirements.txt`]
           # Some older versions of Sphinx (e.g., Sphinx v1.1.3 in
           # RHEL 7):
           #
           # - Don't support "--version".
           # - But do emit the version number as part of the general
           #   CLI help when they don't recognize the --version CLI
           #   option.
           #
           # In that case, we only want the first line, and we want to
           # strip off the leading "v" from the version number.
           #
           # In the case where --version *is* recognized, all the
           # additional processing is harmless and we still end up
           # with the Sphinx version number.
           oac_sphinx_found_version=`$SPHINX_BUILD --version 2>&1 | head -n 1 | cut -d\  -f2 | sed -e 's/^v//'`
           AC_MSG_CHECKING([if Sphinx version is high enough ($oac_sphinx_found_version >= $oac_sphinx_target_version)])
           AS_VERSION_COMPARE([$oac_sphinx_found_version],
                              [$oac_sphinx_target_version],
                              [oac_sphinx_result=lesser],
                              [oac_sphinx_result=equal],
                              [oac_sphinx_result=greater])
           AS_IF([test "$oac_sphinx_result" = "lesser"],
                 [SPHINX_BUILD=
                  AC_MSG_RESULT([no])],
                 [ # If we're building, we're also installing, regardless of
                   # whether we found pre-build docs or not (above).
                  oac_install_docs=1
                  AC_MSG_RESULT([yes])])
          ])

    # If we found Sphinx, check to ensure that we have all the things
    # required to build Open MPI/PRRTE/OpenPMIx-like documentation
    # (e.g., any required pip modules).  If we can't render a sample
    # OMPI-like doc, we're not going to automatically install any
    # missing pip modules; we'll just mark Sphinx as being
    # unavailable.
    AS_IF([test -n "$SPHINX_BUILD"],
          [AC_MSG_CHECKING([for required Sphinx modules])
           oac_startdir=`pwd`
           oac_tmpdir=conftmp.$$
           rm -rf $oac_tmpdir
           mkdir $oac_tmpdir
           cd $oac_tmpdir
           cat > conf.py <<EOF
# Minimum config that we need for Open MPI/PRRTE/OpenPMIx-like docs
project = 'Testing'
copyright = 'Testing'
author = 'Testing'
import sphinx_rtd_theme
# Note the extra quoting needed for square brackets because this is m4
extensions = [[ 'recommonmark', 'sphinx_rtd_theme', 'sphinx.ext.extlinks' ]]
html_theme = 'sphinx_rtd_theme'
EOF
           echo "Hello world" > index.rst

           # Try to render this trivial RST project as both HTML and
           # man pages and see if it works.
           oac_happy=0
           OAC_LOG_COMMAND([$SPHINX_BUILD -M html . build-html],
               [OAC_LOG_COMMAND([$SPHINX_BUILD -M man . build-man],
                   [oac_happy=1])])
           AS_IF([test $oac_happy -eq 1],
                 [AC_MSG_RESULT([found])],
                 [SPHINX_BUILD=
                  AC_MSG_RESULT([not found])])

           cd $oac_startdir
           rm -rf $oac_tmpdir
          ])

    AS_IF([test -z "$SPHINX_BUILD"],
          _oac_program_prefix[_MAKEDIST_DISABLE="$]_oac_program_prefix[_MAKEDIST_DISABLE Sphinx/Documentation"
           AC_MSG_NOTICE([Could not find a suitable sphinx-build on your system.])
           AS_IF([test -n "$3"],
                 [AC_MSG_NOTICE([If you want to build the documentation, ensure that the])
                  AC_MSG_NOTICE([Python modules in $3])
                  AC_MSG_NOTICE([are available.])
                 ])
           AC_MSG_NOTICE([You will not be able to build a distribution tarball.])
          ])

    AS_IF([test $oac_install_docs -eq 0],
          [AC_MSG_WARN([*** You will not have documentation installed.])
           AS_IF([test -n "$2"],
                 [AC_MSG_WARN([*** See the following URL for more information:])
                  AC_MSG_WARN([*** $2])])
          ])

    # If --enable-sphinx was specified and we did not find Sphinx,
    # abort.  This is likely only useful to prevent "oops!" moments
    # from developers.
    AS_IF([test -z "$SPHINX_BUILD" && test "$enable_sphinx" = "yes"],
          [AC_MSG_WARN([A suitable Sphinx was not found, but --enable-sphinx was specified])
           AC_MSG_ERROR([Cannot continue])])

    # Construct a summary message.  Due SUMMARY_ADD's implementation,
    # do *not* include a comma.
    AS_IF([test -n "$SPHINX_BUILD"],
          [ # If we found Sphinx, we always both build and install.
           oac_summary_msg="building and installing"],
          [AS_IF([test $oac_install_docs -eq 1],
                 [oac_summary_msg="installing packaged docs"],
                 [oac_summary_msg="no documentation available"])])

    OAC_SUMMARY_ADD([Miscellaneous], [HTML docs and man pages],
                    [$oac_summary_msg])

    AM_CONDITIONAL(_oac_program_prefix[_BUILD_DOCS], [test -n "$SPHINX_BUILD"])
    AM_CONDITIONAL(_oac_program_prefix[_INSTALL_DOCS], [test $oac_install_docs -eq 1])

    OAC_VAR_SCOPE_POP
])

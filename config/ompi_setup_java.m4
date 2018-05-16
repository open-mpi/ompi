dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2006 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2012 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2007-2012 Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2008-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2013      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl _OMPI_SETUP_JAVA()
dnl ----------------
dnl Invoked by OMPI_SETUP_JAVA only if --enable-mpi-java was specified.
AC_DEFUN([_OMPI_SETUP_JAVA],[
    OPAL_VAR_SCOPE_PUSH([ompi_java_bad ompi_java_found ompi_java_dir ompi_java_jnih ompi_java_PATH_save ompi_java_CPPFLAGS_save])

    # Check for bozo case: ensure a directory was specified
    AS_IF([test "$with_jdk_dir" = "yes" || test "$with_jdk_dir" = "no"],
          [AC_MSG_WARN([Must specify a directory name for --with-jdk-dir])
           AC_MSG_ERROR([Cannot continue])])
    AS_IF([test "$with_jdk_bindir" = "yes" || test "$with_jdk_bindir" = "no"],
          [AC_MSG_WARN([Must specify a directory name for --with-jdk-bindir])
           AC_MSG_ERROR([Cannot continue])])
    AS_IF([test "$with_jdk_headers" = "yes" || test "$with_jdk_headers" = "no"],
          [AC_MSG_WARN([Must specify a directory name for --with-jdk-headers])
           AC_MSG_ERROR([Cannot continue])])

    # Check for bozo case: either specify --with-jdk-dir or
    # (--with-jdk-bindir, --with-jdk-headers) -- not both.
    ompi_java_bad=0
    AS_IF([test -n "$with_jdk_dir" && \
           (test -n "$with_jdk_bindir" || test -n "$with_jdk_headers")],
          [ompi_java_bad=1])
    AS_IF([(test -z "$with_jdk_bindir" && test -n "$with_jdk_headers") || \
           (test -n "$with_jdk_bindir" && test -z "$with_jdk_headers")],
          [ompi_java_bad=1])
    AS_IF([test $ompi_java_bad -eq 1],
          [AC_MSG_WARN([Either specify --with-jdk-dir or both of (--with-jdk_bindir, --with-jdk-headers) -- not both.])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test -n "$with_jdk_dir"],
          [with_jdk_bindir=$with_jdk_dir/bin
           with_jdk_headers=$with_jdk_dir/include])

    ##################################################################
    # with_jdk_dir can now be ignored; with_jdk_bindir and
    # with_jdk_headers will be either empty or have valid values.
    ##################################################################

    # Some java installations are in obscure places.  So let's
    # hard-code a few of the common ones so that users don't have to
    # specify --with-java-<foo>=LONG_ANNOYING_DIRECTORY.
    AS_IF([test -z "$with_jdk_bindir"],
          [ # OS X/macOS
           ompi_java_found=0
           # The following logic was deliberately decided upon in
           # https://github.com/open-mpi/ompi/pull/5015 specifically
           # to prevent this script and the rest of Open MPI's build
           # system from getting confused by the somewhat unorthodox
           # Java toolchain layout present on OS X/macOS systems,
           # described in depth by
           # https://github.com/open-mpi/ompi/pull/5015#issuecomment-379324639,
           # and mishandling OS X/macOS Java toolchain path detection
           # as a result.
           AS_IF([test -x /usr/libexec/java_home],
                 [ompi_java_dir=`/usr/libexec/java_home`],
                 [ompi_java_dir=/System/Library/Frameworks/JavaVM.framework/Versions/Current])
           AC_MSG_CHECKING([for Java in OS X/macOS locations])
           AS_IF([test -d $ompi_java_dir],
                 [AC_MSG_RESULT([found ($ompi_java_dir)])
                  ompi_java_found=1
                  if test -d "$ompi_java_dir/Headers" && test -d "$ompi_java_dir/Commands"; then
                      with_jdk_headers=$ompi_java_dir/Headers
                      with_jdk_bindir=$ompi_java_dir/Commands
                  elif test -d "$ompi_java_dir/include" && test -d "$ompi_java_dir/bin"; then
                      with_jdk_headers=$ompi_java_dir/include
                      with_jdk_bindir=$ompi_java_dir/bin
                  else
                      AC_MSG_WARN([No recognized OS X/macOS JDK directory structure found under $ompi_java_dir])
                      ompi_java_found=0
                  fi],
                 [AC_MSG_RESULT([not found])])

           if test "$ompi_java_found" = "0"; then
               # Various Linux
               if test -z "$JAVA_HOME"; then
                   ompi_java_dir='/usr/lib/jvm/java-*-openjdk-*/include/'
               else
                   ompi_java_dir=$JAVA_HOME/include
               fi
               ompi_java_jnih=`ls $ompi_java_dir/jni.h 2>/dev/null | head -n 1`
               AC_MSG_CHECKING([for Java in Linux locations])
               AS_IF([test -r "$ompi_java_jnih"],
                     [with_jdk_headers=`dirname $ompi_java_jnih`
                      OPAL_WHICH([javac], [with_jdk_bindir])
                      AS_IF([test -n "$with_jdk_bindir"],
                            [AC_MSG_RESULT([found ($with_jdk_headers)])
                             ompi_java_found=1
                             with_jdk_bindir=`dirname $with_jdk_bindir`],
                            [with_jdk_headers=])],
                     [ompi_java_dir='/usr/lib/jvm/default-java/include/'
                      ompi_java_jnih=`ls $ompi_java_dir/jni.h 2>/dev/null | head -n 1`
                      AS_IF([test -r "$ompi_java_jnih"],
                            [with_jdk_headers=`dirname $ompi_java_jnih`
                             OPAL_WHICH([javac], [with_jdk_bindir])
                             AS_IF([test -n "$with_jdk_bindir"],
                                   [AC_MSG_RESULT([found ($with_jdk_headers)])
                                    ompi_java_found=1
                                    with_jdk_bindir=`dirname $with_jdk_bindir`],
                                   [with_jdk_headers=])],
                             [AC_MSG_RESULT([not found])])])
           fi

           if test "$ompi_java_found" = "0"; then
               # Solaris
               ompi_java_dir=/usr/java
               AC_MSG_CHECKING([for Java in Solaris locations])
               AS_IF([test -d $ompi_java_dir && test -r "$ompi_java_dir/include/jni.h"],
                     [AC_MSG_RESULT([found ($ompi_java_dir)])
                      with_jdk_headers=$ompi_java_dir/include
                      with_jdk_bindir=$ompi_java_dir/bin
                      ompi_java_found=1],
                     [AC_MSG_RESULT([not found])])
           fi
          ],
          [ompi_java_found=1])

    if test "$ompi_java_found" = "1"; then
        OPAL_CHECK_WITHDIR([jdk-bindir], [$with_jdk_bindir], [javac])
        OPAL_CHECK_WITHDIR([jdk-headers], [$with_jdk_headers], [jni.h])

        # Look for various Java-related programs
        ompi_java_happy=no
        ompi_java_PATH_save=$PATH
        AS_IF([test -n "$with_jdk_bindir" && test "$with_jdk_bindir" != "yes" && test "$with_jdk_bindir" != "no"],
              [PATH="$with_jdk_bindir:$PATH"])
        AC_PATH_PROG(JAVAC, javac)
        AC_PATH_PROG(JAR, jar)
        AC_PATH_PROG(JAVADOC, javadoc)
        AC_PATH_PROG(JAVAH, javah)
        PATH=$ompi_java_PATH_save

        # Check to see if we have all 3 programs.
        AS_IF([test -z "$JAVAC" || test -z "$JAR" || test -z "$JAVADOC"],
              [ompi_java_happy=no],
              [ompi_java_happy=yes])

        # Look for jni.h
        AS_IF([test "$ompi_java_happy" = "yes"],
              [ompi_java_CPPFLAGS_save=$CPPFLAGS
               # silence a stupid Mac warning
               CPPFLAGS="$CPPFLAGS -DTARGET_RT_MAC_CFM=0"
               AC_MSG_CHECKING([javac -h])
               cat > Conftest.java << EOF
public final class Conftest {
    public native void conftest();
}
EOF
               AS_IF([$JAVAC -d . -h . Conftest.java > /dev/null 2>&1],
                     [AC_MSG_RESULT([yes])],
                     [AC_MSG_RESULT([no])
                      AS_IF([test -n "$JAVAH"],
                            [ompi_javah_happy=yes],
                            [ompi_java_happy=no])])
               rm -f Conftest.java Conftest.class Conftest.h

               AS_IF([test -n "$with_jdk_headers" && test "$with_jdk_headers" != "yes" && test "$with_jdk_headers" != "no"],
                     [OMPI_JDK_CPPFLAGS="-I$with_jdk_headers"
                      # Some flavors of JDK also require -I<blah>/linux.
                      # See if that's there, and if so, add a -I for that,
                      # too.  Ugh.
                      AS_IF([test -d "$with_jdk_headers/linux"],
                            [OMPI_JDK_CPPFLAGS="$OMPI_JDK_CPPFLAGS -I$with_jdk_headers/linux"])
                      # Solaris JDK also require -I<blah>/solaris.
                      # See if that's there, and if so, add a -I for that,
                      # too.  Ugh.
                      AS_IF([test -d "$with_jdk_headers/solaris"],
                            [OMPI_JDK_CPPFLAGS="$OMPI_JDK_CPPFLAGS -I$with_jdk_headers/solaris"])
                      # Darwin JDK also require -I<blah>/darwin.
                      # See if that's there, and if so, add a -I for that,
                      # too.  Ugh.
                      AS_IF([test -d "$with_jdk_headers/darwin"],
                            [OMPI_JDK_CPPFLAGS="$OMPI_JDK_CPPFLAGS -I$with_jdk_headers/darwin"])

                      CPPFLAGS="$CPPFLAGS $OMPI_JDK_CPPFLAGS"])
               AC_CHECK_HEADER([jni.h], [],
                               [ompi_java_happy=no])
               CPPFLAGS=$ompi_java_CPPFLAGS_save
              ])
    else
        ompi_java_happy=no
    fi
    AC_SUBST(OMPI_JDK_CPPFLAGS)

    # Are we happy?
    AC_MSG_CHECKING([if Java support available])
    AS_IF([test "$ompi_java_happy" = "yes"],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])
           AC_MSG_WARN([Java support requested but not found.])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_VAR_SCOPE_POP
])

dnl OMPI_SETUP_JAVA()
dnl ----------------
dnl Do everything required to setup the Java compiler.
AC_DEFUN([OMPI_SETUP_JAVA],[
    OPAL_VAR_SCOPE_PUSH([ompi_java_happy ompi_javah_happy])

    ompi_java_happy=no
    ompi_javah_happy=no

    AC_ARG_WITH(jdk-dir,
                AC_HELP_STRING([--with-jdk-dir(=DIR)],
                               [Location of the JDK header directory.  If you use this option, do not specify --with-jdk-bindir or --with-jdk-headers.]))
    AC_ARG_WITH(jdk-bindir,
                AC_HELP_STRING([--with-jdk-bindir(=DIR)],
                               [Location of the JDK bin directory.  If you use this option, you must also use --with-jdk-headers (and you must NOT use --with-jdk-dir)]))
    AC_ARG_WITH(jdk-headers,
                AC_HELP_STRING([--with-jdk-headers(=DIR)],
                               [Location of the JDK header directory.  If you use this option, you must also use --with-jdk-bindir (and you must NOT use --with-jdk-dir)]))

    # Only setup the compiler if we were requested to
    AS_IF([test "$1" = "yes"],
          [_OMPI_SETUP_JAVA])

    AM_CONDITIONAL(OMPI_HAVE_JAVAH_SUPPORT, test "$ompi_javah_happy" = "yes")

    OPAL_VAR_SCOPE_POP
])

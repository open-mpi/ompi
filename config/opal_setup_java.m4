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
dnl Copyright (c) 2008-2013 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2013      Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OPAL_SETUP_JAVA_BANNER],[
    ompi_show_subtitle "Java compiler" 
])

# OPAL_SETUP_JAVA()
# ----------------
# Do everything required to setup the Java compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OPAL_SETUP_JAVA],[
    AC_REQUIRE([OPAL_SETUP_JAVA_BANNER])

    OPAL_VAR_SCOPE_PUSH([opal_java_bad opal_java_found opal_java_dir opal_java_jnih opal_java_PATH_save opal_java_CPPFLAGS_save])
    AC_ARG_ENABLE(java,
                  AC_HELP_STRING([--enable-java],
                                 [Enable Java-based support in the system - use this option to disable all Java-based compiler tests (default: enabled)]))

    AC_ARG_WITH(jdk-dir,
    AC_HELP_STRING([--with-jdk-dir(=DIR)],
                   [Location of the JDK header directory.  If you use this option, do not specify --with-jdk-bindir or --with-jdk-headers.]))
    AC_ARG_WITH(jdk-bindir,
    AC_HELP_STRING([--with-jdk-bindir(=DIR)],
                   [Location of the JDK bin directory.  If you use this option, you must also use --with-jdk-headers (and you must NOT use --with-jdk-dir)]))
    AC_ARG_WITH(jdk-headers,
    AC_HELP_STRING([--with-jdk-headers(=DIR)],
                   [Location of the JDK header directory.  If you use this option, you must also use --with-jdk-bindir (and you must NOT use --with-jdk-dir)]))

    if test "$enable_java" = "no"; then
        HAVE_JAVA_SUPPORT=0
        opal_java_happy=no
    else
        # Check for bozo case: ensure a directory was specified
        AS_IF([test "$with_jdk_dir" = "yes" -o "$with_jdk_dir" = "no"],
              [AC_MSG_WARN([Must specify a directory name for --with-jdk-dir])
               AC_MSG_ERROR([Cannot continue])])
        AS_IF([test "$with_jdk_bindir" = "yes" -o "$with_jdk_bindir" = "no"],
              [AC_MSG_WARN([Must specify a directory name for --with-jdk-bindir])
               AC_MSG_ERROR([Cannot continue])])
        AS_IF([test "$with_jdk_headers" = "yes" -o "$with_jdk_headers" = "no"],
              [AC_MSG_WARN([Must specify a directory name for --with-jdk-headers])
               AC_MSG_ERROR([Cannot continue])])

        # Check for bozo case: either specify --with-jdk-dir or
        # (--with-jdk-bindir, --with-jdk-headers) -- not both.
        opal_java_bad=0
        AS_IF([test -n "$with_jdk_dir" -a -n "$with_jdk_bindir" -o \
                    -n "$with_jdk_dir" -a -n "$with_jdk_headers"],
              [opal_java_bad=1])
        AS_IF([test -z "$with_jdk_bindir" -a -n "$with_jdk_headers" -o \
                    -n "$with_jdk_bindir" -a -z "$with_jdk_headers"],
              [opal_java_bad=1])
        AS_IF([test "$opal_java_bad" = "1"],
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
              [ # OS X Snow Leopard and Lion (10.6 and 10.7 -- did not
                # check prior versions)
               opal_java_found=0
               opal_java_dir=/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers
               AC_MSG_CHECKING([OSX locations])
               AS_IF([test -d $opal_java_dir],
                     [AC_MSG_RESULT([found ($opal_java_dir)])
                      opal_java_found=1
                      with_jdk_headers=$opal_java_dir 
                      with_jdk_bindir=/usr/bin],
                     [AC_MSG_RESULT([not found])])

               if test "$opal_java_found" = "0"; then
                   # Various Linux
                   if test -z "$JAVA_HOME"; then
                       opal_java_dir='/usr/lib/jvm/java-*-openjdk-*/include/'
                   else
                       opal_java_dir=$JAVA_HOME/include
                   fi
                   opal_java_jnih=`ls $opal_java_dir/jni.h 2>/dev/null | head -n 1`
                   AC_MSG_CHECKING([Linux locations])
                   AS_IF([test -r "$opal_java_jnih"], 
                         [with_jdk_headers=`dirname $opal_java_jnih`
                          OPAL_WHICH([javac], [with_jdk_bindir])
                          AS_IF([test -n "$with_jdk_bindir"],
                                [AC_MSG_RESULT([found ($with_jdk_headers)])
                                 opal_java_found=1
                                 with_jdk_bindir=`dirname $with_jdk_bindir`],
                                [with_jdk_headers=])],
                         [opal_java_dir='/usr/lib/jvm/default-java/include/'
                          opal_java_jnih=`ls $opal_java_dir/jni.h 2>/dev/null | head -n 1`
                          AS_IF([test -r "$opal_java_jnih"], 
                                [with_jdk_headers=`dirname $opal_java_jnih`
                                 OPAL_WHICH([javac], [with_jdk_bindir])
                                 AS_IF([test -n "$with_jdk_bindir"],
                                       [AC_MSG_RESULT([found ($with_jdk_headers)])
                                        opal_java_found=1
                                        with_jdk_bindir=`dirname $with_jdk_bindir`],
                                       [with_jdk_headers=])],
                                 [AC_MSG_RESULT([not found])])])
               fi

               if test "$opal_java_found" = "0"; then
                   # Solaris
                   opal_java_dir=/usr/java
                   AC_MSG_CHECKING([Solaris locations])
                   AS_IF([test -d $opal_java_dir -a -r "$opal_java_dir/include/jni.h"], 
                         [AC_MSG_RESULT([found ($opal_java_dir)])
                          with_jdk_headers=$opal_java_dir/include
                          with_jdk_bindir=$opal_java_dir/bin
                          opal_java_found=1],
                         [AC_MSG_RESULT([not found])])
               fi
              ],
              [opal_java_found=1])

        if test "$opal_java_found" = "1"; then
            OMPI_CHECK_WITHDIR([jdk-bindir], [$with_jdk_bindir], [javac])
            OMPI_CHECK_WITHDIR([jdk-headers], [$with_jdk_headers], [jni.h])

            # Look for various Java-related programs
            opal_java_happy=no
            opal_java_PATH_save=$PATH
            AS_IF([test -n "$with_jdk_bindir" -a "$with_jdk_bindir" != "yes" -a "$with_jdk_bindir" != "no"], 
                  [PATH="$with_jdk_bindir:$PATH"])
            AC_PATH_PROG(JAVAC, javac)
            AC_PATH_PROG(JAVAH, javah)
            AC_PATH_PROG(JAR, jar)
            PATH=$opal_java_PATH_save

            # Check to see if we have all 3 programs.
            AS_IF([test -z "$JAVAC" -o -z "$JAVAH" -o -z "$JAR"],
                  [opal_java_happy=no
                   HAVE_JAVA_SUPPORT=0],
                  [opal_java_happy=yes
                   HAVE_JAVA_SUPPORT=1])

            # Look for jni.h
            AS_IF([test "$opal_java_happy" = "yes"],
                  [opal_java_CPPFLAGS_save=$CPPFLAGS
                   # silence a stupid Mac warning
                   CPPFLAGS="$CPPFLAGS -DTARGET_RT_MAC_CFM=0"
                   AS_IF([test -n "$with_jdk_headers" -a "$with_jdk_headers" != "yes" -a "$with_jdk_headers" != "no"],
                         [OPAL_JDK_CPPFLAGS="-I$with_jdk_headers"
                          # Some flavors of JDK also require -I<blah>/linux.
                          # See if that's there, and if so, add a -I for that,
                          # too.  Ugh.
                          AS_IF([test -d "$with_jdk_headers/linux"],
                                [OPAL_JDK_CPPFLAGS="$OPAL_JDK_CPPFLAGS -I$with_jdk_headers/linux"])
                          # Solaris JDK also require -I<blah>/solaris.
                          # See if that's there, and if so, add a -I for that,
                          # too.  Ugh.
                          AS_IF([test -d "$with_jdk_headers/solaris"],
                                [OPAL_JDK_CPPFLAGS="$OPAL_JDK_CPPFLAGS -I$with_jdk_headers/solaris"])

                          CPPFLAGS="$CPPFLAGS $OPAL_JDK_CPPFLAGS"])
                   AC_CHECK_HEADER([jni.h], [], 
                                   [opal_java_happy=no])
                   CPPFLAGS=$opal_java_CPPFLAGS_save
                  ])
        else
            opal_java_happy=no;
            HAVE_JAVA_SUPPORT=no;
        fi
        AC_SUBST(OPAL_JDK_CPPFLAGS)
    fi

   # Are we happy?
    AC_MSG_CHECKING([Java support available])
    AS_IF([test "$opal_java_happy" = "no"],
          [AC_MSG_RESULT([no])],
          [AC_MSG_RESULT([yes])])

    AC_DEFINE_UNQUOTED([OPAL_HAVE_JAVA_SUPPORT], [$HAVE_JAVA_SUPPORT], [do we have Java support])
    AM_CONDITIONAL(OPAL_HAVE_JAVA_SUPPORT, test "$opal_java_happy" = "yes")
    OPAL_VAR_SCOPE_POP
])

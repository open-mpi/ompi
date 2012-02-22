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
dnl Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_JAVA_BANNER],[
    ompi_show_subtitle "Java compiler" 
])

# OMPI_SETUP_JAVA()
# ----------------
# Do everything required to setup the Java compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OMPI_SETUP_JAVA],[
    AC_REQUIRE([OMPI_SETUP_JAVA_BANNER])

    AC_MSG_CHECKING([if want Java bindings])
    AC_ARG_ENABLE(mpi-java,
        AC_HELP_STRING([--enable-mpi-java],
                       [enable Java MPI bindings (default: enabled)]))

    # Only build the Java bindings if requested
    if test "$enable_mpi_java" = "yes"; then
        AC_MSG_RESULT([yes])
        WANT_MPI_JAVA_SUPPORT=1
        AC_MSG_CHECKING([if shared libraries are enabled])
        AS_IF([test "$enable_shared" != "yes"],
              [AC_MSG_RESULT([no])
               AS_IF([test "$enable_mpi_java" = "yes"],
                     [AC_MSG_WARN([Java bindings cannot be built without shared libraries])
                      AC_MSG_ERROR([Cannot continue])],
                     [AC_MSG_WARN([Java bindings will not build as they require --enable-shared])
                      WANT_MPI_JAVA_SUPPORT=0])],
              [AC_MSG_RESULT([yes])])
    else
        AC_MSG_RESULT([no])
        WANT_MPI_JAVA_SUPPORT=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_WANT_JAVA_BINDINGS], [$WANT_MPI_JAVA_SUPPORT],
                       [do we want java mpi bindings])
    AM_CONDITIONAL(OMPI_WANT_JAVA_BINDINGS, test "$WANT_MPI_JAVA_SUPPORT" = "1")

    AC_ARG_WITH(jdk-dir,
    AC_HELP_STRING([--with-jdk-dir(=DIR)],
                   [Location of the JDK header directory.  If you use this option, do not specify --with-jdk-bindir or --with-jdk-headers.]))
    AC_ARG_WITH(jdk-bindir,
    AC_HELP_STRING([--with-jdk-bindir(=DIR)],
                   [Location of the JDK bin directory.  If you use this option, you must also use --with-jdk-headers (and you must NOT use --with-jdk-dir)]))
    AC_ARG_WITH(jdk-headers,
    AC_HELP_STRING([--with-jdk-headers(=DIR)],
                   [Location of the JDK header directory.  If you use this option, you must also use --with-jdk-bindir (and you must NOT use --with-jdk-dir)]))

    # Check for bozo case: ensue a directory was specified
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
    bad=0
    AS_IF([test -n "$with_jdk_dir" -a -n "$with_jdk_bindir" -o \
                -n "$with_jdk_dir" -a -n "$with_jdk_headers"],[bad=1])
    AS_IF([test -z "$with_jdk_bindir" -a -n "$with_jdk_headers" -o \
                -n "$with_jdk_bindir" -a -z "$with_jdk_headers"],[bad=1])
    AS_IF([test "$bad" = "1"],
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
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1" -a -z "$with_jdk_dir" \
          -a -z "$with_jdk_dir" -a -z "$with_jdk_bindir"],
          [ # OS X Snow Leopard and Lion (10.6 and 10.7 -- did not
            # check prior versions)
           dir=/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers
           AS_IF([test -d $dir], [with_jdk_headers=$dir 
                                  with_jdk_bindir=/usr/bin])

            # Various Linux
            dir='/usr/lib/jvm/java-*-openjdk-*/include/'
            jnih=`ls $dir/jni.h 2>/dev/null | head -n 1`
            AS_IF([test -r "$jnih"], 
                  [with_jdk_headers=`dirname $jnih`
                   OPAL_WHICH([javac], [with_jdk_bindir])
                   AS_IF([test -n "$with_jdk_bindir"],
                         [with_jdk_bindir=`dirname $with_jdk_bindir`],
                         [with_jdk_headers=])],
                  [dir='/usr/lib/jvm/default-java/include/'
                   jnih=`ls $dir/jni.h 2>/dev/null | head -n 1`
                   AS_IF([test -r "$jnih"], 
                         [with_jdk_headers=`dirname $jnih`
                          OPAL_WHICH([javac], [with_jdk_bindir])
                          AS_IF([test -n "$with_jdk_bindir"],
                                [with_jdk_bindir=`dirname $with_jdk_bindir`],
                                [with_jdk_headers=])])])

            # If we think we found them, announce
            AS_IF([test -n "$with_jdk_headers" -a "$with_jdk_bindir"],
                  [AC_MSG_NOTICE([guessing that JDK headers are in $with_jdk_headers])
                   AC_MSG_NOTICE([guessing that JDK javac is in $with_jdk_bindir])])
          ])

   # Find javac and jni.h
   AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1"],
         [OMPI_CHECK_WITHDIR([jdk-bindir], [$with_jdk_bindir], [javac])
          OMPI_CHECK_WITHDIR([jdk-headers], [$with_jdk_headers], [jni.h])])

    # Look for various Java-related programs
    ompi_java_happy=no
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1"], 
          [PATH_save=$PATH
           AS_IF([test -n "$with_jdk_bindir" -a "$with_jdk_bindir" != "yes" -a "$with_jdk_bindir" != "no"], 
           [PATH="$PATH:$with_jdk_bindir"])
           AC_PATH_PROG(JAVAC, javac)
           AC_PATH_PROG(JAVAH, javah)
           AC_PATH_PROG(JAR, jar)
           PATH=$PATH_save

           # Check to see if we have all 3 programs.
           AS_IF([test -z "$JAVAC" -o -z "$JAVAH" -o -z "$JAR"],
                 [ompi_java_happy=no],
                 [ompi_java_happy=yes])
          ])

    # Look for jni.h
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1" -a "$ompi_java_happy" = "yes"],
          [CPPFLAGS_save=$CPPFLAGS
           AS_IF([test -n "$with_jdk_headers" -a "$with_jdk_headers" != "yes" -a "$with_jdk_headers" != "no"],
                 [OMPI_JDK_CPPFLAGS="-I$with_jdk_headers"
                  # Some flavors of JDK also require -I<blah>/linux.
                  # See if that's there, and if so, add a -I for that,
                  # too.  Ugh.
                  AS_IF([test -d "$with_jdk_headers/linux"],
                        [OMPI_JDK_CPPFLAGS="$OMPI_JDK_CPPFLAGS -I$with_jdk_headers/linux"])
                  CPPFLAGS="$CPPFLAGS $OMPI_JDK_CPPFLAGS"])
           AC_CHECK_HEADER([jni.h], [], 
                           [ompi_java_happy=no])
           CPPFLAGS=$CPPFLAGS_save
          ])
    AC_SUBST(OMPI_JDK_CPPFLAGS)

   # Check for pinning support
   # Uncomment when ready (or delete if we don't want it)
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1" -a "$ompi_java_happy" = "yes"],
          [dnl OMPI_JAVA_CHECK_PINNING
           echo ======we should check for java pinning support here...
          ])

   # Are we happy?
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1" -a "$ompi_java_happy" = "no"],
          [AC_MSG_WARN([Java MPI bindings requested, but unable to find proper support])
           AC_MSG_ERROR([Cannot continue])])
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1"],
          [AC_MSG_WARN([******************************************************])
           AC_MSG_WARN([*** Java MPI bindings are provided on a provisional])
           AC_MSG_WARN([*** basis.  They are NOT part of the current or])
           AC_MSG_WARN([*** proposed MPI standard.  Continued inclusion of])
           AC_MSG_WARN([*** the Java MPI bindings in Open MPI is contingent])
           AC_MSG_WARN([*** upon user interest and developer support.])
           AC_MSG_WARN([******************************************************])
          ])

    AC_CONFIG_FILES([
        ompi/mpi/java/Makefile
        ompi/mpi/java/java/Makefile
        ompi/mpi/java/c/Makefile
    ])
])

###########################################################################

AC_DEFUN([OMPI_JAVA_CHECK_PINNING],[
###
dnl testing if Java GC supports pinning
###
AC_MSG_CHECKING(whether Java garbage collector supports pinning)

######################
# JMS This has not been touched yet.  It needs to be OMPI-ified.
# Change to AC_DEFINE (instead of the AC_SUBST of DEFPINS at the end)
######################

changequote(,)

cat > conftest.java <<END
public class conftest {
    public static void main(String [] args) {
        System.loadLibrary("conftest") ;
        int a [] = new int [100] ;
        System.exit(isCopy(a) ? 1 : 0) ;
    }

    static native boolean isCopy(int [] a) ;
}
END

cat > conftest.c <<END
#include "conftest.h"
int p_xargc ; char **p_xargv ;  /* Stop AIX linker complaining */
jboolean JNICALL Java_conftest_isCopy(JNIEnv* env, jclass cls, jintArray a) {

    jboolean isCopy ;
    (*env)->GetIntArrayElements(env, a, &isCopy) ;
    return isCopy ;
}
END

# For AIX shared object generation:
cat > conftest.exp <<END
Java_conftest_isCopy
END

changequote([,])

$JAVA/bin/javac -classpath . conftest.java
$JAVA/bin/javah -classpath . -jni conftest

# Following are hacks... should find cc, etc by autoconf mechanisms
cc -I$JAVA/include -I$JAVA/include/$JOS -c conftest.c
case $target in
    *aix* )
        cc -G -bE:conftest.exp -o libconftest.so conftest.o
        ;;
    *)
        cc $LDFLAG -o libconftest.so conftest.o
        ;;
esac


if $JAVA/bin/java -cp "." -Djava.library.path="." conftest
then
  GC_SUPPORTS_PINNING=yes
else
  GC_SUPPORTS_PINNING=no
fi

AC_MSG_RESULT($GC_SUPPORTS_PINNING)

rm -f conftest.* libconftest.so

if test "$GC_SUPPORTS_PINNING" = "yes" 
then
  DEFPINS=-DGC_DOES_PINNING
fi

AC_SUBST(DEFPINS)
])

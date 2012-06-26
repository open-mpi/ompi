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
dnl Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_JAVA_BINDINGS_BANNER],[
    ompi_show_subtitle "Java MPI bindings" 
])

#
# OMPI_SETUP_NO_JAVA_BINDINGS()
# -----------------------------
# Force configure to not build bindings.
# For the case: ./autogen.pl --no-orte
#
AC_DEFUN([OMPI_SETUP_NO_JAVA_BINDINGS],[
    AC_MSG_CHECKING([if want Java bindings])
    AC_MSG_RESULT([no (Needs ORTE support)])

    WANT_MPI_JAVA_SUPPORT=0

    AC_DEFINE_UNQUOTED([OMPI_WANT_JAVA_BINDINGS], [$WANT_MPI_JAVA_SUPPORT],
                       [do we want java mpi bindings])
    AM_CONDITIONAL(OMPI_WANT_JAVA_BINDINGS, test "$WANT_MPI_JAVA_SUPPORT" = "1")
])

# OMPI_SETUP_JAVA_BINDINGS()
# ----------------
# Do everything required to setup the Java MPI bindings.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OMPI_SETUP_JAVA_BINDINGS],[

    # must have Java setup
    AC_REQUIRE([ORTE_SETUP_JAVA])

    AC_REQUIRE([OMPI_SETUP_JAVA_BINDINGS_BANNER])

    AC_MSG_CHECKING([if want Java bindings])
    AC_ARG_ENABLE(mpi-java,
        AC_HELP_STRING([--enable-mpi-java],
                       [enable Java MPI bindings (default: disabled)]))

    # check for required support
    if test "$orte_java_happy" = "no" -a "$enable_mpi_java" = "yes"; then
        AC_MSG_RESULT([yes])
        AC_MSG_WARN([Java bindings requested but no Java support found])
        AC_MSG_ERROR([cannot continue])
    fi

    # Only build the Java bindings if requested
    if test "$orte_java_happy" = "yes" -a "$enable_mpi_java" = "yes"; then
        AC_MSG_RESULT([yes])
        WANT_MPI_JAVA_SUPPORT=1
        AC_MSG_CHECKING([if shared libraries are enabled])
        AS_IF([test "$enable_shared" != "yes"],
              [AC_MSG_RESULT([no])
               AC_MSG_WARN([Java bindings cannot be built without shared libraries])
               AC_MSG_WARN([Please reconfigure with --enable-shared])
               AC_MSG_ERROR([Cannot continue])],
              [AC_MSG_RESULT([yes])])
        # must have Java support
        AC_MSG_CHECKING([if Java support was found])
        AS_IF([test "$orte_java_happy" = "yes"],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_WARN([Java MPI bindings requested, but Java support was not found])
               AC_MSG_WARN([Please reconfigure the --with-jdk options to where Java])
               AC_MSG_WARN([support can be found])
               AC_MSG_ERROR([Cannot continue])])
    else
        AC_MSG_RESULT([no])
        WANT_MPI_JAVA_SUPPORT=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_WANT_JAVA_BINDINGS], [$WANT_MPI_JAVA_SUPPORT],
                       [do we want java mpi bindings])
    AM_CONDITIONAL(OMPI_WANT_JAVA_BINDINGS, test "$WANT_MPI_JAVA_SUPPORT" = "1")

   # Check for pinning support
   # Uncomment when ready (or delete if we don't want it)
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1"],
          [dnl OMPI_JAVA_CHECK_PINNING
           echo ======we should check for java pinning support here...
          ])

   # Are we happy?
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

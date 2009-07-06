dnl 	have the vtf3 dirs specified
AC_DEFUN([CHECK_VTF3],
[
    vtf3_error="no"
    check_vtf3="yes"
    force_vtf3="no"
    have_vtf3="no"

    AH_TEMPLATE(HAVE_VTF3, [], [defined if vtf3 library is to be used])

    AC_ARG_WITH([vtf3],
        AC_HELP_STRING([--with-vtf3],
            [use vtf3, default: yes if found by configure]),
        [if test "$withval" = "yes"; then force_vtf3="yes"; else check_vtf3="no"; fi])

    AC_ARG_WITH([vtf3-dir],
        AC_HELP_STRING([--with-vtf3-dir],
            [give the path for vtf3, default: /usr]),
        [vtf3_dir="$withval/"])

    AC_ARG_WITH([vtf3-inc-dir],
        AC_HELP_STRING([--with-vtf3-inc-dir],
            [give the path dir vtf3-include files, default: VTF3DIR/include]),
        [vtf3_inc_dir="$withval/"],
        [if test x"$vtf3_dir" != x; then vtf3_inc_dir="$vtf3_dir"include/; fi])

    AC_ARG_WITH([vtf3-lib-dir],
        AC_HELP_STRING([--with-vtf3-lib-dir],
            [give the path for VTF3-libraries, default: VTF3DIR/lib]),
        [vtf3_lib_dir="$withval/"],
        [if test x"$vtf3_dir" != x; then vtf3_lib_dir="$vtf3_dir"lib/; fi])

    AC_ARG_WITH([vtf3-lib],
        AC_HELP_STRING([--with-vtf3-lib],
            [use given vtf3, default: -lvtf3]),
        [vtf3_lib="$withval"])

    if test "$check_vtf3" = "yes"; then
	sav_CPPFLAGS=$CPPFLAGS
	if test x"$vtf3_inc_dir" != x; then
		CPPFLAGS="$CPPFLAGS -I$vtf3_inc_dir"
	fi
        AC_CHECK_HEADER([vtf3.h], [],
        [
            AC_MSG_NOTICE([error: no vtf3.h found; check path for VTF3 package first...])
            vtf3_error="yes"
        ])
	CPPFLAGS=$sav_CPPFLAGS

        if test x"$vtf3_lib" = x -a "$vtf3_error" = "no"; then
            sav_LIBS=$LIBS
            cl="-lvtf3"
            if test x"$vtf3_lib_dir" != x; then
                cl="-L$vtf3_lib_dir $cl"
            fi
            LIBS="$LIBS $cl"
            AC_MSG_CHECKING([whether linking with -lvtf3 works])
            AC_TRY_LINK([],[],
            [AC_MSG_RESULT([yes]); vtf3_lib=-lvtf3],[AC_MSG_RESULT([no])])
            LIBS=$sav_LIBS
        fi

        if test x"$vtf3_lib" = x -a "$vtf3_error" = "no"; then
            AC_MSG_NOTICE([error: no libvtf3 found; check path for VTF3 package first...])
            vtf3_error="yes"
        fi

        if test $vtf3_error = "no"; then
            AC_DEFINE(HAVE_VTF3)
            have_vtf3="yes"
        fi
   fi

    VTF3_LIB_DIR=$vtf3_lib_dir
    VTF3_LIB_LINE=$vtf3_lib
    if test x"$vtf3_lib_dir" != x; then
        VTF3_LIB_LINE="-L$vtf3_lib_dir $VTF3_LIB_LINE"
    fi

    VTF3_INCLUDE_DIR=$vtf3_inc_dir
    VTF3_INCLUDE_LINE=
    if test x"$vtf3_inc_dir" != x; then
        VTF3_INCLUDE_LINE="-I$vtf3_inc_dir"
    fi

    AC_SUBST(VTF3_LIB_DIR)
    AC_SUBST(VTF3_LIB_LINE)
    AC_SUBST(VTF3_INCLUDE_DIR)
    AC_SUBST(VTF3_INCLUDE_LINE)
])
dnl 	have the zlib dirs specified
AC_DEFUN([CHECK_ZLIB],
[
    zlib_error="no"
    check_zlib="yes"
    force_zlib="no"
    have_zlib="no"

    AH_TEMPLATE(HAVE_ZLIB, [], [defined if zlib library is to be used])

    AC_ARG_WITH([zlib],
        AC_HELP_STRING([--with-zlib],
            [use zlib, default: yes if found by configure]),
        [if test "$withval" = "yes"; then force_zlib="yes"; else check_zlib="no"; fi])

    AC_ARG_WITH([zlib-dir],
        AC_HELP_STRING([--with-zlib-dir],
            [give the path for zlib, default: /usr]),
        [zlib_dir="$withval/"])

    AC_ARG_WITH([zlib-inc-dir],
        AC_HELP_STRING([--with-zlib-inc-dir],
            [give the path dir zlib-include files, default: ZLIBDIR/include]),
        [zlib_inc_dir="$withval/"],
        [if test x"$zlib_dir" != x; then zlib_inc_dir="$zlib_dir"include/; fi])

    AC_ARG_WITH([zlib-lib-dir],
        AC_HELP_STRING([--with-zlib-lib-dir],
            [give the path for ZLIB-libraries, default: ZLIBDIR/lib]),
        [zlib_lib_dir="$withval/"],
        [if test x"$zlib_dir" != x; then zlib_lib_dir="$zlib_dir"lib/; fi])

    AC_ARG_WITH([zlib-lib],
        AC_HELP_STRING([--with-zlib-lib],
            [use given zlib, default: -lz]),
        [zlib_lib="$withval"])

    if test "$check_zlib" = "yes"; then
	sav_CPPFLAGS=$CPPFLAGS
	if test x"$zlib_inc_dir" != x; then
                CPPFLAGS="$CPPFLAGS -I$zlib_inc_dir"
        fi
        AC_CHECK_HEADER([zlib.h], [],
        [
            AC_MSG_NOTICE([error: no zlib.h found; check path for ZLIB package first...])
            zlib_error="yes"
        ])
	CPPFLAGS=$sav_CPPFLAGS

        if test x"$zlib_lib" = x -a "$zlib_error" = "no"; then
            sav_LIBS=$LIBS
            cl="-lz"
            if test x"$zlib_lib_dir" != x; then
                cl="-L$zlib_lib_dir $cl"
            fi
            LIBS="$LIBS $cl"
            AC_MSG_CHECKING([whether linking with -lz works])
            AC_TRY_LINK([],[],
            [AC_MSG_RESULT([yes]); zlib_lib=-lz],[AC_MSG_RESULT([no])])
            LIBS=$sav_LIBS
        fi

        if test x"$zlib_lib" = x -a "$zlib_error" = "no"; then
            AC_MSG_NOTICE([error: no libz found; check path for ZLIB package first...])
            zlib_error="yes"
        fi

        if test $zlib_error = "no"; then
            AC_DEFINE(HAVE_ZLIB)
            have_zlib="yes"
        fi
   fi

    ZLIB_LIB_DIR=$zlib_lib_dir
    ZLIB_LIB_LINE=$zlib_lib
    if test x"$zlib_lib_dir" != x; then
        ZLIB_LIB_LINE="-L$zlib_lib_dir $ZLIB_LIB_LINE"
    fi

    ZLIB_INCLUDE_DIR=$zlib_inc_dir
    ZLIB_INCLUDE_LINE=
    if test x"$zlib_inc_dir" != x; then
        ZLIB_INCLUDE_LINE="-I$zlib_inc_dir"
    fi

    AC_SUBST(ZLIB_LIB_DIR)
    AC_SUBST(ZLIB_LIB_LINE)
    AC_SUBST(ZLIB_INCLUDE_DIR)
    AC_SUBST(ZLIB_INCLUDE_LINE)
])
dnl 	switch on/off additonal debug output in otf lib
AC_DEFUN([WITH_DEBUG],
[
    AH_TEMPLATE(OTF_DEBUG, [], [switch on/off additonal debug checks in otf lib])

    AC_ARG_WITH([debug],
        AC_HELP_STRING([--with-debug],[additonal debug checks]),
            [debug=$withval],
            [debug=""])

    if test "$debug" = yes; then
     AC_DEFINE(OTF_DEBUG)
    fi
])

dnl 	switch on/off additonal debug output in otf lib
AC_DEFUN([WITH_VERBOSE],
[
    AH_TEMPLATE(OTF_VERBOSE, [], [switch on/off verbose output in otf lib])

    AC_ARG_WITH([verbose],
        AC_HELP_STRING([--with-verbose],[additonal verbose output]),
            [verbose=$withval],
            [verbose=""])

    if test "$verbose" = yes; then
     AC_DEFINE(OTF_VERBOSE)
    fi
])


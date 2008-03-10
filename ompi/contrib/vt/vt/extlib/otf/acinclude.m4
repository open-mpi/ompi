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

AC_DEFUN([SWIG_PYTHON],[

	#
	#enable python bindings
	#
	AC_ARG_ENABLE([python-bindings],
		AC_HELP_STRING([--enable-python-bindings],
			[force generation of Python bindings]),
		[enable_python_bindings="yes"],
		[enable_python_bindings=""])
		
		
	#
	#with python version
	#
	AC_ARG_WITH([python-version],
		AC_HELP_STRING([--with-python-version=VERSION],
			[force a custom python version]),
		[PYTHON_VERSION=$withval])
		
	
	#
	#only check if the user did --enable-python-bindings
	#
	if test x"$enable_python_bindings" = xyes; then

		#
		#test for swig >= 1.3
		#
		AC_PROG_SWIG(1.3)
		
		#
		#test for python-dev
		#
		AC_PYTHON_DEVEL
		
		test "x$1" != "xno" || swig_shadow=" -noproxy"
		
		#
		#set the swig options
		#
		AC_SUBST([SWIG_PYTHON_OPT],[-python])
		
		#
		#set the python compiler flags
		#
		AC_SUBST([SWIG_PYTHON_CPPFLAGS],[$PYTHON_CPPFLAGS])
		
	fi

])
AC_DEFUN([AC_PYTHON_DEVEL],[

	#
	# Allow the use of a (user set) custom python version
	#
	AC_ARG_VAR([PYTHON_VERSION],[The installed Python
		version to use, for example '2.3'. This string
		will be appended to the Python interpreter
		canonical name.])

	AC_PATH_PROG([PYTHON],[python[$PYTHON_VERSION]])
	if test -z "$PYTHON"; then
	
	   AC_MSG_WARN([Cannot find python$PYTHON_VERSION in your system path])
	   PYTHON_VERSION=""
	   
	else


		#
		# Check for Python include path
		#
		AC_MSG_CHECKING([for python include path])
		if test -z "$PYTHON_CPPFLAGS"; then
			python_path=`$PYTHON -c "import distutils.sysconfig; print distutils.sysconfig.get_python_inc();" 2> /dev/null`
			if test -n "${python_path}"; then
				python_path="-I$python_path"
			fi
			PYTHON_CPPFLAGS=$python_path
		fi
		if test -n "$PYTHON_CPPFLAGS"; then
			AC_MSG_RESULT([$PYTHON_CPPFLAGS])
		else
			AC_MSG_RESULT([no])
		fi
		AC_SUBST([PYTHON_CPPFLAGS])
	
		#
		# Check for Python library path
		#
		AC_MSG_CHECKING([for python library path])
		if test -z "$PYTHON_LDFLAGS"; then
			# (makes two attempts to ensure we've got a version number
			# from the interpreter)
			py_version=`$PYTHON -c "from distutils.sysconfig import *; \
				from string import join; \
				print join(get_config_vars('VERSION'))" 2> /dev/null`
			if test "$py_version" == "[None]" -o -z "$py_version"; then
				if test -n "$PYTHON_VERSION"; then
					py_version=$PYTHON_VERSION
				else
					py_version=`$PYTHON -c "import sys; \
						print sys.version[[:3]]"`
				fi
			fi

			PYTHON_LDFLAGS=`$PYTHON -c "from distutils.sysconfig import *; \
				from string import join; \
				print '-L' + get_python_lib(0,1), \
					'-lpython';" 2> /dev/null`$py_version
		fi
		if test ! "$PYTHON_LDFLAGS" == "$py_version"; then
			AC_MSG_RESULT([$PYTHON_LDFLAGS])
		else
			AC_MSG_RESULT([no])
		fi
		AC_SUBST([PYTHON_LDFLAGS])
	
		#
		# Check for site packages
		#
		AC_MSG_CHECKING([for python site-packages path])
		if test -z "$PYTHON_SITE_PKG"; then
			PYTHON_SITE_PKG=`$PYTHON -c "import distutils.sysconfig; \
					print distutils.sysconfig.get_python_lib(0,0);" 2> /dev/null`
		fi
		if test -n "$PYTHON_SITE_PKG"; then
			AC_MSG_RESULT([$PYTHON_SITE_PKG])
		else
			AC_MSG_RESULT([no])
		fi
		AC_SUBST([PYTHON_SITE_PKG])
	
		#
		# libraries which must be linked in when embedding
		#
		AC_MSG_CHECKING(python extra libraries)
		if test -z "$PYTHON_EXTRA_LIBS"; then
		PYTHON_EXTRA_LIBS=`$PYTHON -c "import distutils.sysconfig; \
					conf = distutils.sysconfig.get_config_var; \
					print conf('LOCALMODLIBS'), conf('LIBS')" 2> /dev/null`
		fi
		if test -n "$PYTHON_EXTRA_LIBS"; then
			AC_MSG_RESULT([$PYTHON_EXTRA_LIBS])
		else
			AC_MSG_RESULT([no])
		fi
		AC_SUBST(PYTHON_EXTRA_LIBS)
	
		#
		# linking flags needed when embedding
		#
		AC_MSG_CHECKING(python extra linking flags)
		if test -z "$PYTHON_EXTRA_LDFLAGS"; then
			PYTHON_EXTRA_LDFLAGS=`$PYTHON -c "import distutils.sysconfig; \
				conf = distutils.sysconfig.get_config_var; \
				print conf('LINKFORSHARED')" 2> /dev/null`
		fi
		if test -n "$PYTHON_EXTRA_LDFLAGS"; then
			AC_MSG_RESULT([$PYTHON_EXTRA_LDFLAGS])
		else
			AC_MSG_RESULT([no])
		fi
		AC_SUBST(PYTHON_EXTRA_LDFLAGS)
	
		#
		# final check to see if everything compiles alright
		#
		AC_MSG_CHECKING([consistency of all components of python development environment])
		AC_LANG_PUSH([C])
		# save current global flags
		LIBS="$ac_save_LIBS $PYTHON_LDFLAGS"
		CPPFLAGS="$ac_save_CPPFLAGS $PYTHON_CPPFLAGS"
		AC_TRY_LINK([
			#include <Python.h>
		],[
			Py_Initialize();
		],[pythonexists=yes],[pythonexists=no])
	
		AC_MSG_RESULT([$pythonexists])
	
		if test ! "$pythonexists" = "yes"; then
			AC_MSG_ERROR([Cannot find python development environment. You probably have to install the development version of the python package.])
			PYTHON_VERSION=""
		fi
		
		AC_LANG_POP
		# turn back to default flags
		CPPFLAGS="$ac_save_CPPFLAGS"
		LIBS="$ac_save_LIBS"
		
	
		#get the paths where to install the package
		AC_SUBST([pkgpythondir],
			['${prefix}'/lib/python${py_version}/site-packages/otf])
			
		AC_SUBST([pkgpyexecdir],
			['${exec_prefix}'/lib/python${py_version}/site-packages/otf])
			
	fi

])
AC_DEFUN([AC_PROG_SWIG],[
	AC_PATH_PROG([SWIG],[swig])
	if test -z "$SWIG" ; then
	
		AC_MSG_ERROR([Cannot find 'swig' program.])
		
	elif test -n "$1" ; then
		AC_MSG_CHECKING([for swig version])
		[swig_version=`$SWIG -version 2>&1 | grep 'SWIG Version' | sed 's/.*\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\).*/\1/g'`]
		AC_MSG_RESULT([$swig_version])
		if test -n "$swig_version" ; then
			# Calculate the required version number components
			[required=$1]
			[required_major=`echo $required | sed 's/[^0-9].*//'`]
			if test -z "$required_major" ; then
					[required_major=0]
			fi
			[required=`echo $required | sed 's/[0-9]*[^0-9]//'`]
			[required_minor=`echo $required | sed 's/[^0-9].*//'`]
			if test -z "$required_minor" ; then
					[required_minor=0]
			fi
			[required=`echo $required | sed 's/[0-9]*[^0-9]//'`]
			[required_patch=`echo $required | sed 's/[^0-9].*//'`]
			if test -z "$required_patch" ; then
					[required_patch=0]
			fi
			# Calculate the available version number components
			[available=$swig_version]
			[available_major=`echo $available | sed 's/[^0-9].*//'`]
			if test -z "$available_major" ; then
					[available_major=0]
			fi
			[available=`echo $available | sed 's/[0-9]*[^0-9]//'`]
			[available_minor=`echo $available | sed 's/[^0-9].*//'`]
			if test -z "$available_minor" ; then
					[available_minor=0]
			fi
			[available=`echo $available | sed 's/[0-9]*[^0-9]//'`]
			[available_patch=`echo $available | sed 's/[^0-9].*//'`]
			if test -z "$available_patch" ; then
					[available_patch=0]
			fi
			if test $available_major -ne $required_major \
					-o $available_minor -ne $required_minor \
					-o $available_patch -lt $required_patch ; then
					AC_MSG_ERROR([swig version >= $1 is required. You have $swig_version.])
			else
					swigexists=yes
			fi
		else
				AC_MSG_ERROR([Cannot determine swig version])
		fi
	fi

])


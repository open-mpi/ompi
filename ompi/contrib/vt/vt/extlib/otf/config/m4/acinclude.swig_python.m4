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

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
			if test "$py_version" = "[None]" -o -z "$py_version"; then
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
		if test ! "$PYTHON_LDFLAGS" = "$py_version"; then
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

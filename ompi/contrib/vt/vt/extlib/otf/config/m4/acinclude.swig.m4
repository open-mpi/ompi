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


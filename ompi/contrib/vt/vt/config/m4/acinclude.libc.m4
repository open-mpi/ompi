AC_DEFUN([ACVT_LIBC],
[
	libc_error="no"

	libc_pathname=

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_WITH(libc,
		AC_HELP_STRING([--with-libc=NAME],
		[give the pathname for LIBC, default: automatically by configure]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-libc' not properly set])])
                libc_pathname=$withval
	])

	AC_MSG_CHECKING([for LIBC's pathname])

	AS_IF([test x"$libc_pathname" = x],
	[
		AS_IF([test "$PLATFORM" = "bgp"],
		[
			libc_pathname="/lib/libc.so.6"
		],
		[
			rm -f conftest
			AC_TRY_LINK([], [],
			[
				AS_IF([test -r "conftest"],
				[
					libc_pathname=`ldd conftest 2>/dev/null | grep "libc\." | \
					               sed -e "s/.*=>//"                          \
					                   -e "s/[ [\(].*[\)]]//"                 \
					                   -e "s/[[[:space:]]]//g"              | \
					               head -n1`
				],
				[
					libc_error="yes"
				])
			],
			[
				libc_error="yes"
			])
		])
	])

	AS_IF([test x"$libc_pathname" != x -a x"$libc_error" = "xno"],
	[
		AC_MSG_RESULT([$libc_pathname])
		AC_DEFINE_UNQUOTED([DEFAULT_LIBC_PATHNAME],
		["$libc_pathname"], [default pathname of LIBC])
	],
	[
		AC_MSG_RESULT([unknown])
		AC_MSG_NOTICE([error: could not determine pathname of LIBC])
		libc_error="yes"
	])
])


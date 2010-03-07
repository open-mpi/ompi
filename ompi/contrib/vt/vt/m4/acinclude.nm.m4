AC_DEFUN([ACVT_NM],
[
	AC_REQUIRE([AC_PROG_CC])
	AC_REQUIRE([AC_PROG_NM])

	AS_IF([test x"$NM" != x -a x"$lt_cv_nm_interface" = "xBSD nm"],
	[
		default_nm="$NM"

		AC_TRY_COMPILE([], [],
		[
			opts_to_check="--demangle"
			for opt in $opts_to_check
			do
				AC_MSG_CHECKING([if $NM demangle option $opt works])
				eval "$NM $opt conftest.$ac_objext" >/dev/null 2>&1
				AS_IF([test "$?" = "0"],
				[
					AC_MSG_RESULT([yes])
					default_nm="$default_nm $opt"
					break
				],
				[
					AC_MSG_RESULT([no])
				])
			done

			opts_to_check="--line-numbers"
			for opt in $opts_to_check
			do
				AC_MSG_CHECKING([if $NM line numbers option $opt works])
				eval "$NM $opt conftest.$ac_objext" >/dev/null 2>&1
				AS_IF([test "$?" = "0"],
				[
					AC_MSG_RESULT([yes])
					default_nm="$default_nm $opt"
					break
				],
				[
					AC_MSG_RESULT([no])
				])
			done
		])

		AC_DEFINE_UNQUOTED([DEFAULT_NM], ["$default_nm"],
		[Command to list symbols from object files.])
	])
])


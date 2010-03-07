AC_DEFUN([ACVT_COMPINST],
[
	compinst_error="no"
	check_compinst="yes"
	force_compinst="no"
	build_compinst_gnu="no"
	build_compinst_intel="no"
	build_compinst_pathscale="no"
	build_compinst_pgi="no"
	build_compinst_phat="no"
	build_compinst_xl="no"
	build_compinst_ftrace="no"
	compinst_list=
	compinst_default=

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(compinst,
		AC_HELP_STRING([--enable-compinst=COMPINSTLIST],
			[enable support for compiler instrumentation (gnu,intel,pathscale,pgi,sun,xl,ftrace), default: automatically by configure]),
	[AS_IF([test x"$enableval" = "xno"], [check_compinst="no"], [enable_compinst="$enableval"])])

	AS_IF([test x"$check_compinst" = "xyes"],
	[
		AC_MSG_CHECKING([for compiler instrumentation])

		AS_IF([test x"$enable_compinst" != x], [force_compinst="yes"])
		AS_IF([test x"$enable_compinst" = "xyes"], [enable_compinst=""])
		
		AS_IF([test x"$enable_compinst" != x],
		[
			compinst_list=`echo $enable_compinst | sed -e 's/,/ /g'`
			first="yes"
			for ci in $compinst_list
			do
				case $ci in
					gnu)
						build_compinst_gnu="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="gnu"])
						;;
					intel)
						build_compinst_intel="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="intel"])
						;;
					pathscale)
						build_compinst_pathscale="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="pathscale"])
						;;
					pgi)
						build_compinst_pgi="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="pgi"])
						;;
					sun)
						build_compinst_phat="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="sun"])
						;;
					xl)
						build_compinst_xl="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="xl"])
						;;
					ftrace)
						build_compinst_ftrace="yes"
						AS_IF([test x"$first" = "xyes"],
						[compinst_default="ftrace"])
						;;
					*)
						AC_MSG_ERROR([unknown compiler instrumentation '$ci'!])
						;;
				esac
				first="no"
			done
			AC_MSG_RESULT([skipped (--enable-compinst=$enable_compinst)])
		],
		[
			base_CC=`basename $CC`
			case $base_CC in
				gcc* | scgcc* | ppu-gcc*)
					build_compinst_gnu="yes"
					compinst_default="gnu"
					AC_MSG_RESULT([gnu])
					;;
				icc*)
					compver=`$CC -dumpversion | cut -d '.' -f 1`
					AS_IF([test $compver -ge 10],
					[
						build_compinst_intel="yes"
						compinst_default="intel"
						AC_MSG_RESULT([intel])
					])
					;;
				pathcc* | scpathcc*)
					compver=`$CC -dumpversion`
					compver_major=`echo $compver | cut -d '.' -f 1`
					compver_minor=`echo $compver | cut -d '.' -f 2`
					AS_IF([test $compver_major -ge 3 -a $compver_minor -ge 1],
					[
						build_compinst_intel="yes"
						compinst_default="pathscale"
						AC_MSG_RESULT([pathscale])
					])
					;;
				pgcc*)
					build_compinst_pgi="yes"
					compinst_default="pgi"
					AC_MSG_RESULT([pgi])
					;;
				xlc* | blrts_xlc*)
					build_compinst_xl="yes"
					compinst_default="xl"
					AC_MSG_RESULT([xl])
					;;
				suncc*)
					build_compinst_phat="yes"
					compinst_default="sun"
					AC_MSG_RESULT([sun])
					;;
				cc*)
					compver=`$CC -V 2>&1 | grep "Sun C"`
					AS_IF([test "$?" = "0"],
					[
						build_compinst_phat="yes"
						compinst_default="sun"
						AC_MSG_RESULT([sun])
					])
					;;
				sxcc*)
					build_compinst_ftrace="yes"
					compinst_default="ftrace"
					AC_MSG_RESULT([ftrace])
					;;
				*)
					AC_MSG_NOTICE([error: unknown compiler '$base_CC' for instrumentation!])
					compinst_error="yes"
					;;
			esac

			compinst_list=$compinst_default
		])

		AS_IF([test x"$build_compinst_gnu" = "xyes" -o x"$build_compinst_intel" = "xyes" -o x"$build_compinst_pathscale" = "xyes"],
		[
			ACVT_NM
		])
	])
])


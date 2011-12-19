AC_DEFUN([ACVT_COMPINST],
[
	compinst_error="no"
	check_compinst="yes"
	force_compinst="no"
	have_compinst="no"

	compinst_type=
	compinst_cflags=
	compinst_cxxflags=
	compinst_fflags=
	compinst_fcflags=

	compinst_gnu_cflags="-g -finstrument-functions"
	compinst_gnu_cxxflags="$compinst_gnu_cflags"
	compinst_gnu_fflags="$compinst_gnu_cflags"
	compinst_gnu_fcflags="$compinst_gnu_cflags"

	compinst_pgi_cflags="-Mprof=func"
	compinst_pgi_cxxflags="$compinst_pgi_cflags"
	compinst_pgi_fflags="$compinst_pgi_cflags"
	compinst_pgi_fcflags="$compinst_pgi_cflags"

	compinst_pgi9_cflags="-Minstrument=functions"
	compinst_pgi9_cxxflags="$compinst_pgi9_cflags"
	compinst_pgi9_fflags="$compinst_pgi9_cflags"
	compinst_pgi9_fcflags="$compinst_pgi9_cflags"

	compinst_sun_cflags=""
	compinst_sun_cxxflags=""
	compinst_sun_fflags=""
	compinst_sun_fcflags="-O -Qoption f90comp -phat"

	compinst_xl_cflags="-qdebug=function_trace"
	compinst_xl_cxxflags="$compinst_xl_cflags"
	compinst_xl_fflags="$compinst_xl_cflags"
	compinst_xl_fcflags="$compinst_xl_cflags"

	compinst_necsx_cflags="-ftrace"
	compinst_necsx_cxxflags="$compinst_necsx_cflags"
	compinst_necsx_fflags="$compinst_necsx_cflags"
	compinst_necsx_fcflags="$compinst_necsx_cflags"

	compinst_openuh_cflags="-fb_create inst -fb_type=1 -fb_phase=0 -epilog -OPT:instr_proc"
	compinst_openuh_cxxflags="$compinst_openuh_cflags"
	compinst_openuh_fflags="$compinst_openuh_cflags"
	compinst_openuh_fcflags="$compinst_openuh_cflags"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(compinst,
		AC_HELP_STRING([--enable-compinst=TYPE],
			[enable support for compiler instrumentation (gnu,intel,pathscale,pgi,pgi9,sun,xl,necsx,open64,openuh), default: automatically by configure]),
	[AS_IF([test x"$enableval" = "xno"], [check_compinst="no"], [enable_compinst="$enableval"])])

	AS_IF([test x"$check_compinst" = "xyes"],
	[
		AC_MSG_CHECKING([for compiler instrumentation])

		AS_IF([test x"$enable_compinst" != x], [force_compinst="yes"])
		AS_IF([test x"$enable_compinst" = "xyes"], [enable_compinst=""])
		
		AS_IF([test x"$enable_compinst" != x],
		[
			AC_MSG_RESULT([skipped (--enable-compinst=$enable_compinst)])

			case $enable_compinst in
				gnu | intel | pathscale | open64)
					compinst_type="gnu"
					;;
				pgi)
					compinst_type="pgi"
					;;
				pgi9)
					compinst_type="pgi9"
					;;
				sun)
					compinst_type="sun"
					;;
				xl)
					compinst_type="xl"
					;;
				necsx)	
					compinst_type="necsx"
					;;
				openuh)
					compinst_type="openuh"
					;;
				*)	AC_MSG_ERROR([unknown compiler instrumentation '$enable_compinst'])
					;;
			esac
		],
		[
			base_CC=`basename $CC`
			case $base_CC in
				gcc* | scgcc* | ppu-gcc*)
					compinst_type="gnu"
					AC_MSG_RESULT([gnu])
					;;
				icc*)
					compver=`$CC -dumpversion`
					compver_major=`echo $compver | cut -d '.' -f 1`
					AS_IF([test $compver_major -ge 10],
					[
						compinst_type="gnu"
						AC_MSG_RESULT([gnu (intel)])
					],
					[
						AC_MSG_RESULT([unknown])
						AC_MSG_NOTICE([error: the version of the Intel compiler ($compver) doesn't support instrumentation])
						compinst_error="yes"
					])
					;;
				pathcc*)
					compver=`$CC -dumpversion`
					compver_major=`echo $compver | cut -d '.' -f 1`
					compver_minor=`echo $compver | cut -d '.' -f 2`
					AS_IF([test $compver_major -ge 3 -a $compver_minor -ge 1],
					[
						compinst_type="gnu"
						AC_MSG_RESULT([gnu (pathscale)])
					],
					[
						AC_MSG_RESULT([unknown])
						AC_MSG_NOTICE([error: the version of the PathScale compiler ($compver) doesn't support instrumentation])
						compinst_error="yes"
					])
					;;
				scpathcc*)
					compinst_type="gnu"
					AC_MSG_RESULT([gnu (pathscale)])
					;;
				pgcc*)
					sav_CFLAGS=$CFLAGS
					CFLAGS="$CFLAGS $compinst_pgi9_cflags"
					AC_TRY_COMPILE([], [],
					[
						compinst_type="pgi9"
						AC_MSG_RESULT([pgi9])
					],
					[
						compinst_type="pgi"
						AC_MSG_RESULT([pgi])
					])
					CFLAGS=$sav_CFLAGS
					;;
				xlc* | blrts_xlc* | bgxlc*)
					compinst_type="xl"
					AC_MSG_RESULT([xl])
					;;
				suncc*)
					compinst_type="sun"
					AC_MSG_RESULT([sun])
					;;
				cc*)
					AS_IF([test "$PLATFORM" = "crayxt" -o "$PLATFORM" = "crayxe"],
					[
						for f in -V --version; do
							case `$CC $f 2>&1` in
								*pgcc\ [[1-8]].*)
									compinst_type="pgi"
									AC_MSG_RESULT([pgi])
									;;
								*pgcc\ *)
									compinst_type="pgi9"
									AC_MSG_RESULT([pgi9])
									;;
								*PathScale*)
									compinst_type="gnu"
									AC_MSG_RESULT([gnu (pathscale)])
									;;
								*Intel*)
									compinst_type="gnu"
									AC_MSG_RESULT([gnu (intel)])
									;;
								*gcc\ *)
									compinst_type="gnu"
									AC_MSG_RESULT([gnu])
									;;
							esac
							AS_IF([test x"$compinst_type" != x], [break])
						done
					],
					[
						compver=`$CC -V 2>&1 | grep "Sun C"`
						AS_IF([test "$?" = "0"],
						[
							compinst_type="sun"
							AC_MSG_RESULT([sun])
						])
					])
					;;
				opencc* | uhcc*)
					case `$CC --version 2>&1` in
						OpenUH*)
							compver=`$CC -dumpversion 2>&1`
							compver_major=`echo $compver | cut -d '.' -f 1`
							AS_IF([test $compver_major -ge 4],
							[
								compinst_type="openuh"
								AC_MSG_RESULT([openuh])
							],
							[
								compinst_type="gnu"
								AC_MSG_RESULT([gnu (open64)])
							])
							;;
						*)
							compinst_type="gnu"
							AC_MSG_RESULT([gnu (open64)])
							;;
					esac
					;;
				sxcc*)
					compinst_type="necsx"
					AC_MSG_RESULT([necsx])
					;;
				*)
					;;
			esac

			AS_IF([test x"$compinst_error" = "xno" -a x"$compinst_type" = x],
			[
				AC_MSG_RESULT([unknown])
				AC_MSG_NOTICE([error: the compiler '$base_CC' doesn't support instrumentation])
				compinst_error="yes"
			])
		])

		AS_IF([test x"$compinst_error" = "xno"],
		[
			AS_IF([test x"$compinst_type" = "xgnu" -o x"$compinst_type" = "xpgi9"],
			[
				ACVT_DL
			])
		])

		AS_IF([test x"$compinst_error" = "xno" -a x"$compinst_type" != x],
		[
			have_compinst="yes"

			case $compinst_type in
				gnu)
					compinst_cflags=$compinst_gnu_cflags
					compinst_cxxflags=$compinst_gnu_cxxflags
					compinst_fflags=$compinst_gnu_fflags
					compinst_fcflags=$compinst_gnu_fcflags
					;;
				pgi)
					compinst_cflags=$compinst_pgi_cflags
					compinst_cxxflags=$compinst_pgi_cxxflags
					compinst_fflags=$compinst_pgi_fflags
					compinst_fcflags=$compinst_pgi_fcflags
					;;
				pgi9)
					compinst_cflags=$compinst_pgi9_cflags
					compinst_cxxflags=$compinst_pgi9_cxxflags
					compinst_fflags=$compinst_pgi9_fflags
					compinst_fcflags=$compinst_pgi9_fcflags
					;;
				sun)
					compinst_cflags=$compinst_sun_cflags
					compinst_cxxflags=$compinst_sun_cxxflags
					compinst_fflags=$compinst_sun_fflags
					compinst_fcflags=$compinst_sun_fcflags
					;;
				xl)
					compinst_cflags=$compinst_xl_cflags
					compinst_cxxflags=$compinst_xl_cxxflags
					compinst_fflags=$compinst_xl_fflags
					compinst_fcflags=$compinst_xl_fcflags
					;;
				necsx)
					compinst_cflags=$compinst_necsx_cflags
					compinst_cxxflags=$compinst_necsx_cxxflags
					compinst_fflags=$compinst_necsx_fflags
					compinst_fcflags=$compinst_necsx_fcflags
					;;
				openuh)
					compinst_cflags=$compinst_openuh_cflags
					compinst_cxxflags=$compinst_openuh_cxxflags
					compinst_fflags=$compinst_openuh_fflags
					compinst_fcflags=$compinst_openuh_fcflags
					;;
			esac
		])
	])
])


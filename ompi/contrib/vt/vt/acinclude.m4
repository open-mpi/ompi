AC_DEFUN([ACVT_BFD],
[
	bfd_error="no"
	check_bfd="yes"
        force_bfd="no"
	have_bfd="no"

	BFDDIR=
	BFDINCDIR=
	BFDLIBDIR=
	BFDLIB=

	AC_ARG_WITH(bfd,
		AC_HELP_STRING([--with-bfd], [use BFD to get symbol information of an executable instrumented with GNU, Intel, or Pathscale compiler, default: yes]),
	[AS_IF([test x"$withval" = "xyes"], [force_bfd="yes"], [check_bfd="no"])])

	AC_ARG_WITH(bfd-dir,
		AC_HELP_STRING([--with-bfd-dir=BFDDIR], [give the path for BFD, default: /usr]),
	[BFDDIR="$withval/"])

	AC_ARG_WITH(bfd-inc-dir,
		AC_HELP_STRING([--with-bfd-inc-dir=BFDINCDIR],
		[give the path for BFD-include files, default: BFDDIR/include]),
	[BFDINCDIR="-I$withval/"],
	[AS_IF([test x"$BFDDIR" != x], [BFDINCDIR="-I$BFDDIR"include/])])

	AC_ARG_WITH(bfd-lib-dir,
		AC_HELP_STRING([--with-bfd-lib-dir=BFDLIBDIR],
		[give the path for BFD-libraries, default: BFDDIR/lib]),
	[BFDLIBDIR="-L$withval/"],
	[AS_IF([test x"$BFDDIR" != x], [BFDLIBDIR="-L$BFDDIR"lib/])])

	AC_ARG_WITH(bfd-lib,
		AC_HELP_STRING([--with-bfd-lib=BFDLIB], [use given bfd lib, default: -lbfd]),
	[BFDLIB="$withval"])

	AS_IF([test x"$check_bfd" = "xyes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $BFDINCDIR"
		AC_CHECK_HEADER([bfd.h], [],
		[
			AC_MSG_NOTICE([error: no bfd.h found; check path for BFD package first...])
			bfd_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$BFDLIB" = x -a "$bfd_error" = "no"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $BFDLIBDIR -lbfd"
			AC_MSG_CHECKING([whether linking with -lbfd works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); BFDLIB=-lbfd],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$BFDLIB" = x -a "$bfd_error" = "no"],
		[
			AC_MSG_NOTICE([error: no libbfd found; check path for BFD package first...])
			bfd_error="yes"
		])

		AS_IF([test "$bfd_error" = "no"], [have_bfd="yes"])
	])

	AC_SUBST(BFDINCDIR)
	AC_SUBST(BFDLIBDIR)
	AC_SUBST(BFDLIB)
])

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
			ACVT_BFD
			AS_IF([test x"$bfd_error" = "xyes"],
			[
				AS_IF([test x"$force_bfd" = "xyes"], [exit 1])
				AC_MSG_WARN([no usable BFD found; using nm-output file for addr./symbol mapping])
			],
			[
				AS_IF([test x"$have_bfd" = "xyes"],
				[ACVT_GNUDMGL])
			])
		])
	])
])

AC_DEFUN([ACVT_COMPWRAP],
[
	VT_WRAPPER_EXTRA_CFLAGS=
	VT_WRAPPER_EXTRA_CXXFLAGS=
	VT_WRAPPER_EXTRA_FFLAGS=
	VT_WRAPPER_EXTRA_FCFLAGS=
	VT_WRAPPER_EXTRA_LDFLAGS=
	VT_WRAPPER_EXTRA_LIBS=
	VT_WRAPPER_INCDIR=
	VT_WRAPPER_LIBDIR=
	VT_WRAPPER_OPARI_BIN=
	VT_WRAPPER_AVAIL_INST="manual pomp"
	VT_WRAPPER_DEFAULT_INST=manual

	AC_REQUIRE([ACVT_COMPINST])
	AC_REQUIRE([ACVT_DYNINST])

	AC_ARG_WITH(wrapper-cflags,
		AC_HELP_STRING([--with-wrapper-cflags],
		[extra flags to add to CFLAGS when using vtcc]),
	[VT_WRAPPER_EXTRA_CFLAGS=$withval])

	AC_ARG_WITH(wrapper-cxxflags,
		AC_HELP_STRING([--with-wrapper-cxxflags],
		[extra flags to add to CXXFLAGS when using vtcxx]),
	[VT_WRAPPER_EXTRA_CXXFLAGS=$withval])

	AC_ARG_WITH(wrapper-fflags,
		AC_HELP_STRING([--with-wrapper-fflags],
		[extra flags to add to FFLAGS when using vtf77]),
	[VT_WRAPPER_EXTRA_FFLAGS=$withval])

	AC_ARG_WITH(wrapper-fcflags,
		AC_HELP_STRING([--with-wrapper-fcflags],
		[extra flags to add to FCFLAGS when using vtf90]),
	[VT_WRAPPER_EXTRA_FCFLAGS=$withval])

	AC_ARG_WITH(wrapper-ldflags,
		AC_HELP_STRING([--with-wrapper-ldflags],
		[extra flags to add to LDFLAGS when using wrapper]),
	[VT_WRAPPER_EXTRA_LDFLAGS=$withval])

	AC_ARG_WITH(wrapper-libs,
		AC_HELP_STRING([--with-wrapper-libs],
		[extra flags to add to LIBS when using wrapper]),
	[VT_WRAPPER_EXTRA_LIBS=$withval])

	ACVT_CONF_EXPAND_VARS([$includedir], [VT_WRAPPER_INCDIR])
	AS_IF([test x"$VT_WRAPPER_INCDIR" = "x/usr/include"],
	[VT_WRAPPER_INCDIR=""])
	ACVT_CONF_EXPAND_VARS([$libdir], [VT_WRAPPER_LIBDIR])
	AS_IF([test x"$VT_WRAPPER_LIBDIR" = "x/usr/lib"],
	[VT_WRAPPER_LIBDIR=""])
	ACVT_CONF_EXPAND_VARS([$bindir/opari], [VT_WRAPPER_OPARI_BIN])

	AS_IF([test x"$compinst_list" != x],
	[
		VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST $compinst_list"
		VT_WRAPPER_DEFAULT_INST=$compinst_default
	])

	AS_IF([test x"$have_dyninst" = "xyes"],
	[VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST dyninst"])

	AC_SUBST(VT_WRAPPER_EXTRA_CFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_CXXFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_FFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_FCFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_LDFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_INCDIR)
	AC_SUBST(VT_WRAPPER_LIBDIR)
	AC_SUBST(VT_WRAPPER_OPARI_BIN)
	AC_SUBST(VT_WRAPPER_AVAIL_INST)
	AC_SUBST(VT_WRAPPER_DEFAULT_INST)
])

AC_DEFUN([ACVT_CONF_INIT],
[
	AC_ARG_ENABLE(config-summary,
		AC_HELP_STRING([--enable-config-summary],
			[show summary of configuration, default: enabled]),
		[enable_config_summary="$enableval"], [enable_config_summary="yes"])

	AC_ARG_ENABLE(config-titles,
		AC_HELP_STRING([--enable-config-titles],
			[show titles for each configure section, default: enabled]),
		[enable_config_titles="$enableval"], [enable_config_titles="yes"])
])

AC_DEFUN([ACVT_CONF_TITLE],
[
	AS_IF([test x"$enable_config_titles" = "xyes"],
	[echo; echo "+++ $1"])
])

AC_DEFUN([ACVT_CONF_SUBTITLE],
[
	AS_IF([test x"$enable_config_titles" = "xyes"],
	[echo "--- $1"])
])

AC_DEFUN([ACVT_CONF_EXPAND_VARS],
[
	var=$1

	while :
	do
		$2=`eval echo $var`
		AS_IF([test x"$$2" = "x$var"], [break], [var=$$2])
	done
])

AC_DEFUN([ACVT_CONF_SUMMARY],
[
	AS_IF([test x"$enable_config_summary" = "xyes"],
	[
		echo ""
		echo "-----------------------------------------------------------------------------"
		echo "Configuration:"
		echo ""
		echo "  Build (B):                              $build"
		echo "  Host  (H):                              $host"
		echo "  Cross compiling:                        $cross_compiling"
		echo ""
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  C Compiler (H|B):                       $CC | $CC_FOR_BUILD"],
		[echo "  C Compiler:                             $CC"])
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  C++ Compiler (H|B):                     $CXX | $CXX_FOR_BUILD"],
		[echo "  C++ compiler:                           $CXX"])
		echo "  Fortran 77 compiler:                    $F77"
		echo "  Fortran 90 compiler:                    $FC"
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  C preprocessor (H|B):                   $CPP | $CPP_FOR_BUILD"],
		[echo "  C preprocessor:                         $CPP"])
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  C++ preprocessor (H|B):                 $CXXCPP | $CXXCPP_FOR_BUILD"],
		[echo "  C++ preprocessor:                       $CXXCPP"])
		echo ""
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  CFLAGS (H|B):                           $CFLAGS | $CFLAGS_FOR_BUILD"],
		[echo "  CFLAGS:                                 $CFLAGS"])
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  CXXFLAGS (H|B):                         $CXXFLAGS | $CXXFLAGS_FOR_BUILD"],
		[echo "  CXXFLAGS:                               $CXXFLAGS"])
		echo "  FFLAGS:                                 $FFLAGS"
		echo "  FCFLAGS:                                $FCFLAGS"
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  LDFLAGS (H|B):                          $LDFLAGS | $LDFLAGS_FOR_BUILD"],
		[echo "  LDFLAGS:                                $LDFLAGS"])
		AS_IF([test x"$cross_compiling" = "xyes"],
		[echo "  LIBS (H|B):                             $LIBS | $LIBS_FOR_BUILD"],
		[echo "  LIBS:                                   $LIBS"])
		echo "  OMPFLAG:                                $OMPFLAG"
		echo ""
		echo "  MPI C compiler:                         $MPICC"
		echo "  MPICFLAGS (append to CFLAGS):           $MPICFLAGS"
		echo ""
		echo "  Source code location:                   $PWD"
		echo "  Install path:                           $prefix"
		echo ""
		answer=""
		AS_IF([test x"$use_extern_otf" = "xno"],
		[answer="yes"], [answer="no"])
		echo "  Build OTF library and tools:               $answer"

		AS_IF([test x"$have_zlib" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "   ZLIB trace compression support            $answer"

		AS_IF([test x"$build_mpi" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build MPI support:                         $answer"

		AS_IF([test x"$build_fmpi" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "   Build Fortran MPI wrapper library:        $answer"

		AS_IF([test x"$build_omp" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build OpenMP support:                      $answer"

		AS_IF([test x"$build_hyb" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build Hybrid (MPI/OpenMP) support:         $answer"

		AS_IF([test x"$have_papi" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build PAPI Hardware Counter support:       $answer"

		AS_IF([test x"$have_memhooks" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build GLIBC's memory alloc. trace support: $answer"

		AS_IF([test x"$have_iowrap" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build LIBC's I/O trace support:            $answer"

		AS_IF([test x"$compinst_list" != x],
		[answer="$compinst_list"], [answer="no"])
		echo ""
		echo "  Build compiler instrumentation support:    $answer"
		AS_IF([test x"$compinst_default" != x],
		[answer="$compinst_default"], [answer="manual"])
		echo "   Default for compiler wrapper:             $answer"

		AS_IF([test x"$have_dyninst" = "xyes"],
		[answer="yes"], [answer="no"])
		echo ""
		echo "  Build binary instrumentation support"
		echo "  by using Dyninst:                          $answer"
		AS_IF([test x"$build_dynattlib" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "   Build Dyninst attach library:             $answer"

		echo ""
		echo "  See config.h for further configuration information."
		echo "-----------------------------------------------------------------------------"
	])
])
AC_DEFUN([ACVT_CROSS],
[
	comp_for_build_given="no"

	AC_ARG_VAR(CC_FOR_BUILD, [C compiler command for build system])
	AC_ARG_VAR(CFLAGS_FOR_BUILD, [C compiler flags for build system])
	AC_ARG_VAR(CPP_FOR_BUILD, [C preprocessor for build system])
	AC_ARG_VAR(CXX_FOR_BUILD, [C++ compiler command for build system])
	AC_ARG_VAR(CXXFLAGS_FOR_BUILD, [C++ compiler flags for build system])
	AC_ARG_VAR(CXXCPP_FOR_BUILD, [C++ preprocessor for build system])
	AC_ARG_VAR(CPPFLAGS_FOR_BUILD, [C/C++/Objective C preprocessor flags for build system])
	AC_ARG_VAR(LDFLAGS_FOR_BUILD, [linker flags for build system])
	AC_ARG_VAR(LIBS_FOR_BUILD, [libraries to pass to the linker for build system])

	AS_IF([test x"$cross_compiling" != "xyes"],
	[
		# reset *_FOR_BUILD variables, if no cross compiling
		CC_FOR_BUILD=$CC; CFLAGS_FOR_BUILD=$CFLAGS; CPP_FOR_BUILD=$CPP
		CXX_FOR_BUILD=$CXX; CXXFLAGS_FOR_BUILD=$CXXFLAGS; CXXCPP_FOR_BUILD=$CXXCPP
		CPPFLAGS_FOR_BUILD=$CPPFLAGS; LDFLAGS_FOR_BUILD=$LDFLAGS; LIBS_FOR_BUILD=$LIBS
	],
	[
		AC_MSG_CHECKING([for C compiler for build system ($build)])
		AS_IF([test x"$CC_FOR_BUILD" = x],
		[AC_MSG_RESULT([$CC])], [AC_MSG_RESULT([$CC_FOR_BUILD])])
		AC_MSG_CHECKING([for C++ compiler for build system ($build)])
		AS_IF([test x"$CXX_FOR_BUILD" = x],
		[AC_MSG_RESULT([$CXX])], [AC_MSG_RESULT([$CXX_FOR_BUILD])])

		AS_IF([test x"$CC_FOR_BUILD" = x -a x"$CXX_FOR_BUILD" != x],
		[
			AC_MSG_ERROR([no C compiler command for build system given
Set \`CC_FOR_BUILD' to the C compiler of build system.])
		])
		AS_IF([test x"$CXX_FOR_BUILD" = x -a x"$CC_FOR_BUILD" != x],
		[
			AC_MSG_ERROR([no C++ compiler command for build system given
Set \`CXX_FOR_BUILD' to the C++ compiler of build system.])
		])
		AS_IF([test x"$CC_FOR_BUILD" != x -a x"$CXX_FOR_BUILD" != x],
		[
			comp_for_build_given="yes"
		])
		AS_IF([test x"$CC_FOR_BUILD" = x -a x"$CXX_FOR_BUILD" = x],
		[
			AC_MSG_NOTICE([no C/C++ compiler command for build system given
In cross compile mode, it's recommended to build the compiler wrappers and OPARI for the build system. Set \`CC_FOR_BUILD' and \`CXX_FOR_BUILD' to the C/C++ compiler of build system.])
			CC_FOR_BUILD=$CC
			CXX_FOR_BUILD=$CXX
		])

		AS_IF([test x"$CFLAGS_FOR_BUILD" = x], [CFLAGS_FOR_BUILD=$CFLAGS])
		AS_IF([test x"$CXXFLAGS_FOR_BUILD" = x], [CXXFLAGS_FOR_BUILD=$CXXFLAGS])
		AS_IF([test x"$CPPFLAGS_FOR_BUILD" = x], [CPPFLAGS_FOR_BUILD=$CPPFLAGS])
		AS_IF([test x"$LDFLAGS_FOR_BUILD" = x], [LDFLAGS_FOR_BUILD=$LDFLAGS])
		AS_IF([test x"$LIBS_FOR_BUILD" = x], [LIBS_FOR_BUILD=$LIBS])

		AS_IF([test x"$comp_for_build_given" = "xyes"],
		[
			sav_CPPFLAGS=$CPPFLAGS; sav_LDFLAGS=$LDFLAGS; sav_LIBS=$LIBS
			CPPFLAGS=$CPPFLAGS_FOR_BUILD; LDFLAGS=$LDFLAGS_FOR_BUILD; LIBS=$LIBS_FOR_BUILD

			sav_CC=$CC; sav_CFLAGS=$CFLAGS; sav_CPP=$CPP
			CC=$CC_FOR_BUILD; CFLAGS=$CFLAGS_FOR_BUILD
			AC_MSG_CHECKING([whether the C compiler for build system works])
			AC_TRY_LINK([], [],
			[AC_MSG_RESULT([yes])],
			[AC_MSG_ERROR([C compiler for build system cannot create executables
See \`config.log' for more details.])])

			AS_IF([test x"$CPP_FOR_BUILD" = x],
			[
				unset CPP
				unset ac_cv_prog_CPP # clear cache variable for CPP
				AC_PROG_CPP
				CPP_FOR_BUILD=$CPP
			])
			CC=$sav_CC; CFLAGS=$sav_CFLAGS; CPP=$sav_CPP

			AC_LANG([C++])
			sav_CXX=$CXX; sav_CXXFLAGS=$CXXFLAGS; sav_CXXCPP=$CXXCPP
			CXX=$CXX_FOR_BUILD; CXXFLAGS=$CXXFLAGS_FOR_BUILD
			AC_MSG_CHECKING([whether the C++ compiler for build system works])
			AC_TRY_LINK([], [],
			[AC_MSG_RESULT([yes])],
			[AC_MSG_ERROR([C++ compiler for build system cannot create executables
See \`config.log' for more details.])])

			AS_IF([test x"$CXXCPP_FOR_BUILD" = x],
			[
				unset CXXCPP
				unset ac_cv_prog_CXXCPP # clear cache variable for CXXCPP
				AC_PROG_CXXCPP
				CXXCPP_FOR_BUILD=$CXXCPP
			])
			CXX=$sav_CXX; CXXFLAGS=$sav_CXXFLAGS; CXXCPP=$sav_CXXCPP
			AC_LANG([C])

			CPPFLAGS=$sav_CPPFLAGS; LDFLAGS=$sav_LDFLAGS; LIBS=$sav_LIBS
		])
	])
])

AC_DEFUN([ACVT_DL],
[
	dl_error="no"
	have_dl="no"

	have_rtld_next="no"

	DLDIR=
	DLINCDIR=
	DLLIBDIR=
	DLLIB=

	AC_ARG_WITH(dl-dir,
		AC_HELP_STRING([--with-dl-dir=DLDIR], [give the path for libdl, default: /usr]),
	[DLDIR="$withval/"])

	AC_ARG_WITH(dl-inc-dir,
		AC_HELP_STRING([--with-dl-inc-dir=DLINCDIR],
		[give the path for libdl-include files, default: DLDIR/include]),
	[DLINCDIR="-I$withval/"],
	[AS_IF([test x"$DLDIR" != x], [DLINCDIR="-I$DLDIR"include/])])

	AC_ARG_WITH(dl-lib-dir,
		AC_HELP_STRING([--with-dl-lib-dir=DLLIBDIR],
		[give the path for libdl-libraries, default: DLDIR/lib]),
	[DLLIBDIR="-L$withval/"],
	[AS_IF([test x"$DLDIR" != x], [DLLIBDIR="-L$DLDIR"lib/])])

	AC_ARG_WITH(dl-lib,
		AC_HELP_STRING([--with-dl-lib=DLLIB], [use given libdl lib, default: -ldl]),
	[DLLIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $DLINCDIR"
	AC_CHECK_HEADER([dlfcn.h], [],
	[
		AC_MSG_NOTICE([error: no dlfcn.h found; check path for libdl package first...])
		dl_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$DLLIB" = x -a x"$dl_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $DLLIBDIR -ldl"
		AC_MSG_CHECKING([whether linking with -ldl works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); DLLIB=-ldl],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$DLLIB" = x -a x"$dl_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libdl found; check path for libdl package first...])
		dl_error="yes"
	])

	AS_IF([test x"$dl_error" = "xno"],
	[
		AC_CHECK_DECL([RTLD_NEXT], [have_rtld_next="yes"], [], [#include <dlfcn.h>])

		AS_IF([test x"$have_rtld_next" = "xno"],
		[
			AC_MSG_CHECKING([whether we need to define _GNU_SOURCE to get RTLD_NEXT])
			AC_TRY_COMPILE(
			[
#define _GNU_SOURCE
#include <dlfcn.h>
			],
			[
#ifndef RTLD_NEXT
  (void) RTLD_NEXT;
#endif
			],
			[
				AC_MSG_RESULT([yes])
				have_rtld_next="yes"
				CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
			],
			[
				AC_MSG_RESULT([no])
			])
		])
	])

	AS_IF([test x"$DLLIB" != x -a x"$dl_error" = "xno"], [have_dl="yes"])

	AC_SUBST(DLINCDIR)
	AC_SUBST(DLLIBDIR)
	AC_SUBST(DLLIB)
])

AC_DEFUN([ACVT_DYNINST],
[
	dyninst_error="no"
	dynattlib_error="no"
	check_dyninst="yes"
	force_dyninst="no"
	check_dynattlib="yes"
	force_dynattlib="no"
	build_dynattlib="no"
	have_dyninst="no"

	DYNIDIR=
	DYNIINCDIR=
	DYNILIBDIR=
	DYNILIB=
	VTDYNATTLIB=

	AC_ARG_ENABLE(dyninst,
		AC_HELP_STRING([--enable-dyninst],
		[enable support for binary instrumentation by using Dyninst, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_dyninst="yes"], [check_dyninst="no"])])

	AC_ARG_ENABLE(dyninst-attlib,
		AC_HELP_STRING([--enable-dyninst-attlib],
		[build shared library which attach Dyninst to running user's application, default: enable if Dyninst found by configure and system supports shared libraries]),
	[AS_IF([test x"$enableval" = "xyes"], [force_dyninst="yes"; check_dyninst="yes"; force_dynattlib="yes"; build_dynattlib="yes"], [check_dynattlib="no"])])

	AC_ARG_WITH(dyninst-dir,
		AC_HELP_STRING([--with-dyninst-dir=DYNIDIR], [give the path for Dyninst, default: /usr]),
	[DYNIDIR="$withval/"])

	AC_ARG_WITH(dyninst-inc-dir,
		AC_HELP_STRING([--with-dyninst-inc-dir=DYNIINCDIR],
		[give the path for Dyninst-include files, default: DYNIDIR/include]),
	[DYNIINCDIR="-I$withval/"],
	[AS_IF([test x"$DYNIDIR" != x], [DYNIINCDIR="-I$DYNIDIR"include/])])

	AC_ARG_WITH(dyninst-lib-dir,
		AC_HELP_STRING([--with-dyninst-lib-dir=DYNILIBDIR],
		[give the path for Dyninst-libraries, default: DYNIDIR/lib]),
	[DYNILIBDIR="-L$withval/"],
	[AS_IF([test x"$DYNIDIR" != x], [DYNILIBDIR="-L$DYNIDIR"lib/])])

	AC_ARG_WITH(dyninst-lib,
		AC_HELP_STRING([--with-dyninst-lib=DYNILIB], [use given Dyninst lib, default: -ldyninstAPI -liberty]),
	[DYNILIB="$withval"])

	AS_IF([test "$check_dyninst" = "yes"],
	[
		AS_IF([test x"$liberty_error" = x], [ACVT_LIBERTY])
		AS_IF([test x"$have_liberty" = "xno"], [dyninst_error="yes"])

		AC_LANG([C++])

		AS_IF([test x"$dyninst_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $DYNIINCDIR"
			AC_CHECK_HEADER([BPatch.h], [],
			[
				AC_MSG_NOTICE([error: no BPatch.h found; check path for Dyninst package first...])
				dyninst_error="yes"
			])
			CPPFLAGS=$sav_CPPFLAGS
		])

		AS_IF([test x"$DYNILIB" = x -a x"$dyninst_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $DYNILIBDIR -ldyninstAPI $LIBERTYLIBDIR $LIBERTYLIB"
			AC_MSG_CHECKING([whether linking with -ldyninstAPI $LIBERTYLIB works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); DYNILIB="-ldyninstAPI $LIBERTYLIBDIR $LIBERTYLIB"],
			[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AC_LANG([C])

		AS_IF([test x"$DYNILIB" = x -a x"$dyninst_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libdyninstAPI found; check path for Dyninst package first...])
			dyninst_error="yes"
		])

		AS_IF([test x"$DYNILIB" != x -a x"$dyninst_error" = "xno"],
		[
			have_dyninst="yes"
			AS_IF([test x"$check_dynattlib" = "xyes"],
			[
				ACVT_LDSHFLAG
				AS_IF([test x"$ldshflag_error" = "xno"],
				[
					build_dynattlib="yes"
					VTDYNATTLIB="-lvt.dynatt"
				],
				[
					AC_MSG_NOTICE([error: could not determine linker flag to create shared libraries!])
					dynattlib_error="yes"
				])
			])
		])
	])

	AC_SUBST(DYNIINCDIR)
	AC_SUBST(DYNILIBDIR)
	AC_SUBST(DYNILIB)
	AC_SUBST(VTDYNATTLIB)
])

AC_DEFUN([ACVT_GNUDMGL],
[
	gnudmgl_error="no"

	ACVT_LIBERTY

	AS_IF([test x"$LIBERTYLIB" = x],
	[gnudmgl_error="yes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $LIBERTYINCDIR"
		AC_CHECK_HEADER([demangle.h], [], [gnudmgl_error="yes"])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$gnudmgl_error" = "xno"],
	[
		AC_DEFINE([HAVE_GNU_DEMANGLE],
			[1], [Define to 1 if you can use GNU demangling of C++ names.])
	])
])

AC_DEFUN([ACVT_IOWRAP],
[
	iowrap_error="no"
	check_iowrap="yes"
	force_iowrap="no"
	have_iowrap="no"

	AC_ARG_ENABLE(iotrace, 
		AC_HELP_STRING([--enable-iotrace],
		[enable libc's I/O tracing support, default: enable if libdl found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_iowrap="yes"], [check_iowrap="no"])])

	AS_IF([test x"$check_iowrap" = "xyes"],
	[
		AS_IF([test x"$dl_error" = x], [ACVT_DL])
		AS_IF([test x"$have_dl" = "xno" -o x"$have_rtld_next" = "xno"],
		[iowrap_error="yes"])

		AS_IF([test x"$iowrap_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS -D_LARGEFILE64_SOURCE"
			AC_CHECK_FUNCS([ \
				creat64 \
				fopen64 \
				fseeko \
				fseeko64 \
				lseek64 \
				fsetpos64 \
				open64 \
				pread64 \
				pwrite64])
			CPPFLAGS=$sav_CPPFLAGS

			have_iowrap="yes"
		])
	])
])

AC_DEFUN([ACVT_LDSHFLAG],
[
	ldshflag_error="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_MSG_CHECKING([for linker flag to create shared libraries])

	AC_ARG_VAR(LDSHFLAG, [linker flag to create shared libraries])

	AS_IF([test x"$LDSHFLAG" != x],
	[
		AC_MSG_RESULT([skipped (LDSHFLAG=$LDSHFLAG)])
	],
	[
		LDSHFLAG=

		ldshflag=
		base_CC=`basename $CC`
		case $base_CC in
			gcc* | scgcc* | icc* | pgcc* | pathcc* | scpathcc* | suncc*)
				ldshflag="-shared"
				;;
			xlc* | blrts_xlc*)
				ldshflag="-G"
				;;
			cc*)
				AS_IF([test x"$PLATFORM" = "xmips"],
				[ldshflag="-shared"])
				;;
		esac

		AS_IF([test x"$ldshflag" != x],
		[AC_MSG_RESULT([$ldshflag])], [AC_MSG_RESULT([unknown])])

		LDSHFLAG=$ldshflag
	])

	AS_IF([test x"$LDSHFLAG" = x], [ldshflag_error="yes"])
])

AC_DEFUN([ACVT_LIBERTY],
[
	liberty_error="no"
	have_liberty="no"

	LIBERTYDIR=
	LIBERTYINCDIR=
	LIBERTYLIBDIR=
	LIBERTYLIB=

	AC_ARG_WITH(liberty-dir,
		AC_HELP_STRING([--with-liberty-dir=LIBERTYDIR], [give the path for LIBERTY, default: /usr]),
	[LIBERTYDIR="$withval/"])

	AC_ARG_WITH(liberty-inc-dir,
		AC_HELP_STRING([--with-liberty-inc-dir=LIBERTYINCDIR],
		[give the path for LIBERTY-include files, default: LIBERTYDIR/include]),
	[LIBERTYINCDIR="-I$withval/"],
	[AS_IF([test x"$LIBERTYDIR" != x], [LIBERTYINCDIR="-I$LIBERTYDIR"include/])])

	AC_ARG_WITH(liberty-lib-dir,
		AC_HELP_STRING([--with-liberty-lib-dir=LIBERTYLIBDIR],
		[give the path for LIBERTY-libraries, default: LIBERTYDIR/lib]),
	[LIBERTYLIBDIR="-L$withval/"],
	[AS_IF([test x"$LIBERTYDIR" != x], [LIBERTYLIBDIR="-L$LIBERTYDIR"lib/])])

	AC_ARG_WITH(liberty-lib,
		AC_HELP_STRING([--with-liberty-lib=LIBERTYLIB], [use given liberty lib, default: -liberty]),
	[LIBERTYLIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $LIBERTYINCDIR"
	AC_CHECK_HEADER([libiberty.h], [],
	[
		AC_MSG_NOTICE([error: no libiberty.h found; check path for LIBERTY package first...])
		liberty_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $LIBERTYLIBDIR -liberty"
		AC_MSG_CHECKING([whether linking with -liberty works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); LIBERTYLIB=-liberty],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libiberty found; check path for LIBERTY package first...])
		liberty_error="yes"
	])

	AS_IF([test x"$LIBERTYLIB" != x -a x"$liberty_error" = "xno"],
	[have_liberty="yes"])

	AC_SUBST(LIBERTYINCDIR)
	AC_SUBST(LIBERTYLIBDIR)
	AC_SUBST(LIBERTYLIB)
])

AC_DEFUN([ACVT_LTMPDIR],
[
	AC_ARG_WITH(local-tmp-dir,
                AC_HELP_STRING([--with-local-tmp-dir=LTMPDIR],
			[give the path for node-local temporary directory, default: /tmp]),
		[LTMPDIR="$withval"], [LTMPDIR=""])

	AS_IF([test x"$LTMPDIR" != x],
	[AC_DEFINE_UNQUOTED(PFORM_LDIR, ["$LTMPDIR"], [Path for node-local temporary directory])])
])

AC_DEFUN([ACVT_MEMHOOKS],
[
	memhooks_error="no"
	check_memhooks="yes"
	force_memhooks="no"
	have_memhooks="no"

	AC_ARG_ENABLE(memtrace,
		AC_HELP_STRING([--enable-memtrace], [enable memory tracing support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_memhooks="yes"], [check_memhooks="no"])])

	AS_IF([test x"$check_memhooks" = "xyes"],
	[
		AC_CHECK_HEADER([malloc.h], [],
		[
			AC_MSG_NOTICE([error: no malloc.h found])
			memhooks_error="yes"
		])

		AS_IF([test x"$memhooks_error" = "xno"],
		[
			memhooks_error="yes"
			AC_CHECK_FUNC([__malloc_hook],
			[AC_CHECK_FUNC([__realloc_hook],
			 [AC_CHECK_FUNC([__free_hook], [memhooks_error="no"],
			  [])])])
		])

		AS_IF([test x"$memhooks_error" = "xno"], [have_memhooks="yes"])
	])
])

AC_DEFUN([ACVT_MPI],
[
	mpi_error="no"
	mpi_status_size=
	have_mpithrd=
	have_mpio=

	MPIDIR=
	MPIINCDIR=
	MPILIBDIR=
	MPILIB=
	PMPILIB=
	FMPILIB=

	AC_ARG_VAR(MPICC, [MPI C compiler command])
	AC_ARG_VAR(MPICFLAGS, [MPI C compiler flags (append to CFLAGS)])

	AC_ARG_WITH(mpi-dir,
		AC_HELP_STRING([--with-mpi-dir=MPIDIR], [give the path for MPI, default: /usr]),
	[MPIDIR="$withval/"])

	AC_ARG_WITH(mpi-inc-dir,
		AC_HELP_STRING([--with-mpi-inc-dir=MPIINCDIR],
		[give the path for MPI-include files, default: MPIDIR/include]),
	[MPIINCDIR="-I$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPIINCDIR="-I$MPIDIR"include/])])

	AC_ARG_WITH(mpi-lib-dir,
		AC_HELP_STRING([--with-mpi-lib-dir=MPILIBDIR],
		[give the path for MPI-libraries, default: MPIDIR/lib]),
	[MPILIBDIR="-L$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPILIBDIR="-L$MPIDIR"lib/])])

	AC_ARG_WITH(mpi-lib,
		AC_HELP_STRING([--with-mpi-lib], [use given mpi lib]),
	[MPILIB="$withval"])

	AC_ARG_WITH(pmpi-lib,
		AC_HELP_STRING([--with-pmpi-lib], [use given pmpi lib]),
	[PMPILIB="$withval"])

	AC_ARG_WITH(fmpi-lib,
		AC_HELP_STRING([--with-fmpi-lib], [use given fmpi lib]),
	[FMPILIB="$withval"])

	AC_ARG_WITH(mpi-status-size,
		AC_HELP_STRING([--with-mpi-status-size],
		[give the value of MPI_STATUS_SIZE, default: automatically by configure]),
	[
		AS_IF([test x"$withval" = "x" -o x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-mpi-status-size' not properly set])],
		[mpi_status_size=$withval])
	])

	AC_ARG_ENABLE(mpi-thread,
		AC_HELP_STRING(--enable-mpi-thread,
		[MPI supports threads, default: yes if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [have_mpithrd=yes], [have_mpithrd=no])])

	AC_ARG_ENABLE(mpi-io,
		AC_HELP_STRING(--enable-mpi-io,
		[MPI supports file access, default: yes if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [have_mpio=yes], [have_mpio=no])])

	AC_ARG_ENABLE(fmpi-lib,
		AC_HELP_STRING([--enable-fmpi-lib],
		[build MPI Fortran support library, default: enable if no MPI Fortran library found by configure]), 
	[AS_IF([test x"$enableval" = "xyes"], [FMPILIB="-lvt.fmpi"; build_fmpi="yes"])])

	AC_ARG_WITH(mpi-native,
		AC_HELP_STRING([--with-mpi-native], [configure for computer manufacturer MPI]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpi"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="$MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lvt.fmpi"])
			build_fmpi="yes"
		])
	AC_ARG_WITH(mpich,
		AC_HELP_STRING([--with-mpich], [configure for MPICH]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpich"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="-lpmpich $MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lfmpich"])
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
		])
	AC_ARG_WITH(mpich2,
		AC_HELP_STRING([--with-mpich2], [configure for MPICH2]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpich"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="$MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lfmpich"])
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
		])
	AC_ARG_WITH(lam,
		AC_HELP_STRING([--with-lam], [configure for LAM/MPI]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpi -llam"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="$MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-llamf77mpi"])
		])
	AC_ARG_WITH(openmpi,
		AC_HELP_STRING([--with-openmpi], [configure for Open MPI]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpi"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="$MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lvt.fmpi"])
			have_mpithrd="yes"
			build_fmpi="yes"
		])
	AC_ARG_WITH(hpmpi,
		AC_HELP_STRING([--with-hpmpi], [configure for HP MPI]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmtmpi"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="-lmtpmpi"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lvt.fmpi"])
			build_fmpi="yes"
		])
	AC_ARG_WITH(intelmpi,
		AC_HELP_STRING([--with-intelmpi], [configure for Intel MPI]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpi"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="$MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lmpiif"])
		])
	AC_ARG_WITH(intelmpi2,
		AC_HELP_STRING([--with-intelmpi2], [configure for Intel MPI2]),
		[
			AS_IF([test x"$MPILIB" = x], [MPILIB="-lmpi"])
			AS_IF([test x"$PMPILIB" = x], [PMPILIB="$MPILIB"])
			AS_IF([test x"$FMPILIB" = x], [FMPILIB="-lmpiif"])
		])

	AC_CHECK_PROGS(MPICC, mpicc hcc mpcc mpcc_r mpxlc mpixlc cmpicc mpiicc)

	AS_IF([test x"$MPICC" != x],
	[
		mpicc=`echo $MPICC | cut -d ' ' -f 1`
		which_mpicc=`which $mpicc 2>/dev/null`
		AS_IF([test x"$which_mpicc" = x], [AC_MSG_ERROR([$mpicc not found!])])

		mpi_bin_dir=`dirname $which_mpicc`
		AS_IF([test "$mpi_bin_dir" != "/usr/bin"],
		[
			AS_IF([test x"$MPIDIR" = x],
			[MPIDIR=`echo $mpi_bin_dir | sed -e 's/bin//'`])
			AS_IF([test x"$MPIINCDIR" = x],
			[MPIINCDIR=-I`echo $mpi_bin_dir | sed -e 's/bin/include/'`])
			AS_IF([test x"$MPILIBDIR" = x],
			[MPILIBDIR=-L`echo $mpi_bin_dir | sed -e 's/bin/lib/'`])
		])
	],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $MPIINCDIR"
		AC_CHECK_HEADER([mpi.h], [MPICC="$CC"],
		[
			AC_MSG_NOTICE([error: no mpi.h found; check path for MPI package first...])
			mpi_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

dnl	check for MPILIB

	AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lmpich"
		AC_MSG_CHECKING([whether linking with -lmpich works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); MPILIB=-lmpich],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
		AS_IF([test x"$MPILIB" != x],
		[MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"])
	])

	AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lmpichg2"
		AC_MSG_CHECKING([whether linking with -lmpichg2 works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); MPILIB=-lmpichg2],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
		AS_IF([test x"$MPILIB" != x],
		[MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"])
	])

	AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lhpmpi"
		AC_MSG_CHECKING([whether linking with -lhpmpi works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); MPILIB=-lhpmpi],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
		AS_IF([test x"$MPILIB" != x],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmtmpi"
			AC_MSG_CHECKING([whether linking with -lmtmpi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmtmpi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])
	])

	AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lmpi"
		AC_MSG_CHECKING([whether linking with -lmpi works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); MPILIB=-lmpi],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
		AS_IF([test x"$MPILIB" != x],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -llam"
			AC_MSG_CHECKING([whether linking with -llam works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB="-lmpi -llam"],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])
	])

	AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
	[
		AC_MSG_NOTICE([error: no libhpmpi, libmpich, libmpi, or liblam found; check path for MPI package first...])
		mpi_error="yes"
	])

dnl	check for PMPILIB

	AS_IF([test x"$PMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lpmpich"
		AC_MSG_CHECKING([whether linking with -lpmpich works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); PMPILIB=-lpmpich],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$PMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lpmpichg2"
		AC_MSG_CHECKING([whether linking with -lpmpichg2 works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); PMPILIB=-lpmpichg2],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$PMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lpmpi"
		AC_MSG_CHECKING([whether linking with -lpmpi works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); PMPILIB=-lpmpi],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
		AS_IF([test x"$PMPILIB" != x],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmtpmpi"
			AC_MSG_CHECKING([whether linking with -lmtpmpi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); PMPILIB=-lmtpmpi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])
	])

	AS_IF([test x"$PMPILIB" = x -a "$mpi_error" = "no"],
	[
		PMPILIB="$MPILIB"
		AC_MSG_WARN([no libpmpich, or libpmpi found; assuming $MPILIB])
	])

dnl	check for FMPILIB

	AS_IF([test x"$FMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lfmpich"
		AC_MSG_CHECKING([whether linking with -lfmpich works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); FMPILIB=-lfmpich],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$FMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -llamf77mpi"
		AC_MSG_CHECKING([whether linking with -llamf77mpi works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); FMPILIB=-llamf77mpi],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$FMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lmpiif"
		AC_MSG_CHECKING([whether linking with -lmpiif works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); FMPILIB=-lmpiif],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$FMPILIB" = x -a "$mpi_error" = "no"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $MPILIBDIR -lfmpi"
		AC_MSG_CHECKING([whether linking with -lfmpi works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); FMPILIB=-lfmpi],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$FMPILIB" = x -a "$mpi_error" = "no"],
	[
		FMPILIB="-lvt.fmpi"
		build_fmpi="yes"
		AC_MSG_WARN([no libfmpich, liblamf77mpi, libfmpi, or libmpiif found; build libvt.fmpi.a])
	])

dnl	check for MPI_STATUS_SIZE

	AS_IF([test x"$build_fmpi" = "xyes" -a "$mpi_error" = "no"],
	[
		AC_MSG_CHECKING([for the value of MPI_STATUS_SIZE])
		AS_IF([test x"$mpi_status_size" != x],
		[
			AC_MSG_RESULT([skipped (--with-mpi-status-size=$mpi_status_size)])
			AC_DEFINE_UNQUOTED(MPI_STATUS_SIZE, [$mpi_status_size], [Define as the size of MPI_STATUS_SIZE])
		],
		[
			sav_CC=$CC
			sav_CFLAGS=$CFLAGS
			CC=$MPICC
			CFLAGS="$CFLAGS $MPIINCDIR"
			AC_TRY_RUN(
[
#include <stdio.h>
#include <mpi.h>
int main () {
  FILE * f = fopen( "conftest.val", "w" );
  if( !f ) return 1;
  fprintf( f, "%d", MPI_STATUS_SIZE );
  fclose( f );
  return 0;
}], [mpi_status_size=`cat ./conftest.val`], [], [mpi_status_size=])

			AS_IF([test x"$mpi_status_size" != x],
			[
				AC_MSG_RESULT([$mpi_status_size])],
			[
				AC_TRY_RUN(
[
#include <stdio.h>
#include <mpi.h>
int main () {
  FILE * f;
  int sizeof_mpistatus = sizeof( MPI_Status );
  int sizeof_int = sizeof( int );
  if( sizeof_mpistatus == 0 || sizeof_int == 0 ) return 1;
  f = fopen( "conftest.val", "w" );
  if( !f ) return 1;
  fprintf( f, "%d", sizeof_mpistatus / sizeof_int );
  fclose( f );
  return 0;
}], [mpi_status_size=`cat ./conftest.val`], [], [mpi_status_size=])

				AS_IF([test x"$mpi_status_size" != x],
				[
					AC_MSG_RESULT([$mpi_status_size])
				],
				[
					mpi_status_size=5
					AC_MSG_RESULT([unknown])
					AC_MSG_WARN([could not determine the value of MPI_STATUS_SIZE; assuming $mpi_status_size])
				])
				AC_DEFINE_UNQUOTED(MPI_STATUS_SIZE, [$mpi_status_size], [Define as the size of MPI_STATUS_SIZE])
			])

			rm -f conftest.val
			CC=$sav_CC
			CFLAGS=$sav_CFLAGS
		])
	])

	AS_IF([test "$mpi_error" = "no"],
	[
		sav_CC=$CC
		sav_CFLAGS=$CFLAGS
		sav_LIBS=$LIBS
		CC=$MPICC
		CFLAGS="$CFLAGS $MPIINCDIR"
		LIBS="$LIBS $MPILIBDIR $MPILIB"

dnl		check for MPI Thread support

		AS_IF([test x"$have_mpithrd" = x],
		[
			AC_CHECK_FUNC([MPI_Init_thread],
				[have_mpithrd="yes"], [have_mpithrd="no"])
		])
		AS_IF([test "$have_mpithrd" = "yes"],
		[AC_DEFINE([HAVE_MPITHRD], [1], [Define to 1 if MPI supports threads.])])

dnl		check for MPI I/O

		AS_IF([test x"$have_mpio" = x],
		[
			AC_CHECK_FUNC([MPI_File_open],
				[have_mpio="yes"], [have_mpio="no"])
		])
		AS_IF([test "$have_mpio" = "yes"],
		[AC_DEFINE([HAVE_MPIO], [1], [Define to 1 if MPI supports file access.])])

		CC=$sav_CC
		CFLAGS=$sav_CFLAGS
		LIBS=$sav_LIBS
	])


	AS_IF([test "$mpi_error" = "yes"], [MPICC=$CC])

	AS_IF([test x"$PMPILIB" = x"$MPILIB"], [PMPILIB=])
	
	AC_SUBST(MPIDIR)
	AC_SUBST(MPIINCDIR)
	AC_SUBST(MPILIBDIR)
	AC_SUBST(MPILIB)
	AC_SUBST(PMPILIB)
	AC_SUBST(FMPILIB)
])

AC_DEFUN([ACVT_OMP],
[
	omp_error="no"
	have_omp="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_VAR(OMPFLAG, [compiler flag to enable OpenMP parallelizer])

	AS_IF([test "$PLATFORM" = "bgl"],
	[
		AC_MSG_CHECKING([for OpenMP flag of C compiler])
		AC_MSG_RESULT([skipped (not supported on BlueGene/L)])
		omp_error="yes"
	])

	AS_IF([test "$omp_error" = "no"],
	[
		AS_IF([test x"$OMPFLAG" != x],
		[
			AC_MSG_CHECKING([for OpenMP flag of C compiler])
			AC_MSG_RESULT([skipped (OMPFLAG=$OMPFLAG)])
		],
		[
			AX_OPENMP
			AS_IF([test x"$OPENMP_CFLAGS" = x],
			[omp_error="yes"], [OMPFLAG=$OPENMP_CFLAGS])
		])
	])

	AS_IF([test "$omp_error" = "no"],
	[
		sav_CFLAGS=$CFLAGS
		CFLAGS="$CFLAGS $OMPFLAG"
		AC_CHECK_HEADER([omp.h], [have_omp="yes"], [omp_error="yes"])
		CFLAGS=$sav_CFLAGS
	])
])

AC_DEFUN([ACVT_OTF],
[
	otf_error="no"

	OTFDIR=
	OTFINCDIR=
	OTFLIBDIR=

	AC_REQUIRE([ACVT_ZLIB])

	AC_ARG_WITH(extern-otf,
		AC_HELP_STRING([--with-extern-otf], [use external OTF library, default: not set]),
	[use_extern_otf="yes"], [use_extern_otf="no"])

	AC_ARG_WITH(extern-otf-dir,
		AC_HELP_STRING([--with-extern-otf-dir=OTFDIR], [give the path for OTF, default: /usr]),
	[OTFDIR="$withval/"])

	AC_ARG_WITH(extern-otf-inc-dir,
		AC_HELP_STRING([--with-extern-otf-inc-dir=OTFINCDIR],
		[give the path for OTF-include files, default: OTFDIR/include]),
	[OTFINCDIR="-I$withval/"],
	[AS_IF([test x"$OTFDIR" != x], [OTFINCDIR="-I$OTFDIR"include/])])

	AC_ARG_WITH(extern-otf-lib-dir,
		AC_HELP_STRING([--with-extern-otf-lib-dir=OTFLIBDIR],
		[give the path for OTF-libraries, default: OTFDIR/lib]),
	[OTFLIBDIR="-L$withval/"],
	[AS_IF([test x"$OTFDIR" != x], [OTFLIBDIR="-L$OTFDIR"lib/])])

	AC_ARG_WITH(otf-lib,
		AC_HELP_STRING([--with-otf-lib=OTFLIB], [use given otf lib, default: -lotf -lz]),
	[OTFLIB="$withval"])

	AC_ARG_WITH(otf-flags,
		AC_HELP_STRING([--with-otf-flags=FLAGS], [pass FLAGS to the OTF distribution configuration script]), [OTFFLAGS="$withval"])

	AS_IF([test "$use_extern_otf" = "yes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $OTFINCDIR"
		AC_CHECK_HEADER([otf.h], [],
		[
			AC_MSG_NOTICE([error: no otf.h found; check path for OTF package first...])
			otf_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$OTFLIB" = x -a "$otf_error" = "no"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $OTFLIBDIR -lotf $ZLIBLIBDIR $ZLIBLIB"
			AC_MSG_CHECKING([whether linking with -lotf $ZLIBLIBDIR $ZLIBLIB works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); OTFLIB="-lotf $ZLIBLIBDIR $ZLIBLIB"],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$OTFLIB" = x -a "$otf_error" = "no"],
		[
			AC_MSG_NOTICE([error: no libotf found; check path for OTF package first...])
			otf_error="yes"
		])
	],
	[
		otf_parent_dir=`pwd`
		otf_dir="extlib/otf"

		AC_MSG_NOTICE([configuring in $otf_dir ($otf_parent_dir/$otf_dir)])

		AS_IF([test "$srcdir" != "."],
		[
			test -d "$otf_dir" ||
			mkdir -p "$otf_dir" ||
			AC_MSG_ERROR([cannot create $otf_dir])
		])

		cd $otf_dir

		case $srcdir in
			.)
				otf_srcdir="$srcdir"
				;;
			/*)
				otf_srcdir="$srcdir/$otf_dir"
				;;
			*)
				otf_srcdir="../../$srcdir/$otf_dir"
				;;
		esac

		otf_conf_cmd="$otf_srcdir/configure"
		otf_conf_args=
		AS_IF([test x"$enable_binaries" != "xyes"],
		[
			otf_conf_args="$otf_conf_args --disable-binaries"
		])
		AS_IF([test x"$cross_compiling" = "xyes"],
		[
			AS_IF([test ! -z $build], [otf_conf_args="$otf_conf_args --build=$build"])
			AS_IF([test ! -z $host], [otf_conf_args="$otf_conf_args --host=$host"])
		])
		AS_IF([test x"$have_zlib" = "xyes"],
		[
			AS_IF([test x"$force_zlib" = "xyes"],
			[otf_conf_args="$otf_conf_args --with-zlib"])
			AS_IF([test x"$zlib_dir_withval" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-dir=$zlib_dir_withval"])
			AS_IF([test x"$zlib_incdir_withval" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-inc-dir=$zlib_incdir_withval"])
			AS_IF([test x"$zlib_libdir_withval" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-lib-dir=$zlib_libdir_withval"])
			AS_IF([test x"$ZLIBLIB" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-lib=$ZLIBLIB"])
		],
		[
			otf_conf_args="$otf_conf_args --without-zlib"
		])

		otf_conf_args="$otf_conf_args --prefix=\"$prefix\" --exec-prefix=\"$exec_prefix\" --bindir=\"$bindir\" --libdir=\"$libdir\" --includedir=\"$includedir\" --docdir=\"$docdir/otf\" $OTFFLAGS --cache-file=\"/dev/null\" --srcdir=\"$otf_srcdir\""
		
		AC_MSG_NOTICE([running $SHELL $otf_conf_cmd $otf_conf_args])
		eval "$SHELL '$otf_conf_cmd' $otf_conf_args"
		AS_IF([test $? != "0"], [AC_MSG_ERROR([$otf_conf_cmd failed for $otf_dir])])

		cd $otf_parent_dir

		OTFINCDIR=
		OTFLIBDIR=
		AS_IF([test x"$OTFLIB" = x], [OTFLIB="-lotf $ZLIBLIBDIR $ZLIBLIB"])
	])

	AC_SUBST(OTFDIR)
	AC_SUBST(OTFINCDIR)
	AC_SUBST(OTFLIBDIR)
	AC_SUBST(OTFLIB)
])

AC_DEFUN([ACVT_PAPI],
[
	papi_error="no"
	check_papi="yes"
	force_papi="no"
	have_papi="no"

	PAPIDIR=
	PAPIINCDIR=
	PAPILIBDIR=
	PAPILIB=

	AC_ARG_ENABLE(papi,
		AC_HELP_STRING([--enable-papi],
		[enable PAPI hardware counter support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_papi="yes"], [check_papi="no"])])

	AC_ARG_WITH(papi-dir,
		AC_HELP_STRING([--with-papi-dir=PAPIDIR],
		[give the path for PAPI, default: /usr]),
	[PAPIDIR="$withval/"])

	AC_ARG_WITH(papi-inc-dir,
		AC_HELP_STRING([--with-papi-inc-dir=PAPIINCDIR],
		[give the path for PAPI-include files, default: PAPIDIR/include]),
	[PAPIINCDIR="-I$withval/"],
	[AS_IF([test x"$PAPIDIR" != x], [PAPIINCDIR="-I$PAPIDIR"include/])])

	AC_ARG_WITH(papi-lib-dir,
		AC_HELP_STRING([--with-papi-lib-dir=PAPILIBDIR],
		[give the path for PAPI-libraries, default: PAPIDIR/lib]),
	[PAPILIBDIR="-L$withval/"],
	[AS_IF([test x"$PAPIDIR" != x], [PAPILIBDIR="-L$PAPIDIR"lib/])])

	AC_ARG_WITH(papi-lib,
		AC_HELP_STRING([--with-papi-lib=PAPILIB], [use given papi lib, default: -lpapi]),
	[PAPILIB="$withval"])

	AS_IF([test x"$check_papi" = "xyes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $PAPIINCDIR"
		AC_CHECK_HEADER([papi.h], [],
		[
			AC_MSG_NOTICE([error: no papi.h found; check path for PAPI package first...])
			papi_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$PAPILIB" = x -a x"$papi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $PAPILIBDIR -lpapi"
			AC_MSG_CHECKING([whether linking with -lpapi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); PAPILIB=-lpapi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$PAPILIB" = x -a x"$papi_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libpapi found; check path for PAPI package first...])
			papi_error="yes"
		])

		AS_IF([test x"$papi_error" = "xno"],
		[
			AC_MSG_CHECKING([whether PAPI version = 3.x])

			sav_CFLAGS=$CFLAGS
			CFLAGS="$CFLAGS $PAPIINCDIR"
			AC_TRY_COMPILE([#include <papi.h>],
[
#ifndef PAPI_VERSION
#  error "PAPI_VERSION not defined; version < 3.x"
#elif PAPI_VERSION_MAJOR(PAPI_VERSION) != 3
#  error "PAPI_VERSION_MAJOR != 3"
#endif
],
			[AC_MSG_RESULT([yes])],
			[
				AC_MSG_RESULT([no])
				AC_MSG_NOTICE([error: PAPI version could not be determined and/or is incompatible (!= 3.x)
See \`config.log' for more details.])
				papi_error="yes"
			])
			CFLAGS=$sav_CFLAGS
		])

		AS_IF([test x"$PAPILIB" != x -a x"$papi_error" = "xno"],
		[
			AC_DEFINE([TIMER_PAPI_REAL_CYC], [10], [PAPI_get_real_cyc])
			AC_DEFINE([TIMER_PAPI_REAL_USEC], [11], [PAPI_get_real_usec])
			AS_IF([test x"$pform_timer" = "xTIMER_GETTIMEOFDAY"],
			[
				pform_timer=TIMER_PAPI_REAL_CYC
				AC_DEFINE_UNQUOTED([TIMER], [$pform_timer], [Use timer (see below)])
				AC_MSG_NOTICE([reselected timer: $pform_timer])
			])
			have_papi="yes"
		])
	])

	AC_SUBST(PAPIINCDIR)
	AC_SUBST(PAPILIBDIR)
	AC_SUBST(PAPILIB)
])

AC_DEFUN([ACVT_PLATFORM],
[
	PLATFORM=

	pform_timer=

	AC_MSG_CHECKING([for platform])

	AC_ARG_WITH(platform,
		AC_HELP_STRING([--with-platform], [configure for given platform (altix,ibm,linux,macos,sun,origin,crayxt,generic), default: automatically by configure]),
	[
		PLATFORM=$withval
		AC_MSG_RESULT([skipped (--with-platform=$withval)])
	],
	[
		case $host_os in
			linux*)
				AS_IF([test "$host_cpu" = "ia64" -a -f /etc/sgi-release],
				[PLATFORM=altix],
				[AS_IF([test "$host_cpu" = "powerpc64" -a -d /bgl/BlueLight],
				 [PLATFORM=bgl],
				 [AS_IF([test "$host_cpu" = "x86_64" -a -d /opt/xt-boot],
				  [PLATFORM=crayxt],
				  [PLATFORM=linux])])])
				;;
			sunos* | solaris*)
				PLATFORM=sun
				;;
			darwin*)
				PLATFORM=macos
				;;
			irix*)
				AS_IF([test "$host_cpu" = "mips"], [PLATFORM="origin"])
				;;
			aix*)
				PLATFORM=ibm
				;;
			unicosmp*)
				PLATFORM=crayx1
				;;
			superux*)
				PLATFORM=necsx
				;;
		esac

		AS_IF([test x"$PLATFORM" = x],
		[
			AC_MSG_WARN([unknown platform '$host'! using generic configuration])
			PLATFORM=generic
		],
		[
			AC_MSG_RESULT([$PLATFORM])
		])
	])

	AC_DEFINE([DISABLE_CLOCK_SYNC], [0], [Define to 1 to disable clock synchronization])

	case $PLATFORM in
	linux)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [2], [Use `gettimeofday' function])
		pform_timer=TIMER_GETTIMEOFDAY

		case $host_cpu in
			i*86 | x86* | powerpc*)
				AC_DEFINE([TIMER_CYCLE_COUNTER], [3], [Cycle counter (e.g. TSC)])
				pform_timer=TIMER_CYCLE_COUNTER
				;;
			ia64)
				AC_CHECK_HEADERS([asm/intrinsics.h],
				[
					AC_CHECK_DECL([_IA64_REG_AR_ITC],
					[
						AC_DEFINE([TIMER_CYCLE_COUNTER], [3], [Cycle counter (e.g. ITC)])
						pform_timer=TIMER_CYCLE_COUNTER
					], [], [#include <asm/intrinsics.h>])
				])
				;;
		esac
		;;
	macos)
		AC_DEFINE([TIMER_CYCLE_COUNTER], [1], [Cycle counter (e.g. TSC)])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [2], [Use `gettimeofday' function])
		pform_timer=TIMER_CYCLE_COUNTER
		;;
	altix)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		pform_timer=TIMER_CLOCK_GETTIME

		mmtimer_h_found=no
		AC_CHECK_HEADERS([linux/mmtimer.h], [mmtimer_h_found=yes],
		[AC_CHECK_HEADERS([sn/mmtimer.h], [mmtimer_h_found=yes],
		[AC_CHECK_HEADERS([mmtimer.h], [mmtimer_h_found=yes])])])
		AS_IF([test x"$mmtimer_h_found" = "xyes"],
		[
			AC_CHECK_FILE([/dev/mmtimer],
			[
				AC_DEFINE([TIMER_MMTIMER], [2], [Intel Multimedia Timer])
				pform_timer=TIMER_MMTIMER
			])
		])
		;;
	bgl)
		AC_DEFINE([TIMER_RTS_GET_TIMEBASE], [1], [Read PowerPC 440 time base registers])
		pform_timer=TIMER_RTS_GET_TIMEBASE
		;;
	ibm)
		AC_DEFINE([TIMER_POWER_REALTIME], [1], [IBM Power family Real-Time-Clock])
		AC_DEFINE([TIMER_SWITCH_CLOCK], [2], [Hardware Switch-Clock (it's necessary to link your application with '-lswclock')])
		pform_timer=TIMER_POWER_REALTIME
		;;
	sun)
		AC_DEFINE([TIMER_GETHRTIME], [1], [gethrtime])
		pform_timer=TIMER_GETHRTIME
		;;
	necsx)
		AC_DEFINE([TIMER_SYSSX_HGTIME], [1], [NEC SX HGTIME])
		pform_timer=TIMER_SYSSX_HGTIME
		;;
	crayt3e)
		AC_DEFINE([TIMER_CRAY_RTCLOCK],[1], [CRAY Real-Time-Clock])
		pform_timer=TIMER_CRAY_RTCLOCK
		;;
	crayx1)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		AC_DEFINE([TIMER_RTC], [2], [RTC (DOES NOT WORK YET WITH FORTRAN CODES)])
		pform_timer=TIMER_GETTIMEOFDAY
		;;
	crayxt)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		AC_DEFINE([TIMER_CYCLE_COUNTER], [2], [Cycle counter (e.g. TSC)])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [3], [Use `gettimeofday' function])
		pform_timer=TIMER_CYCLE_COUNTER

		AC_TRY_COMPILE([],
[
#ifndef __LIBCATAMOUNT__
#  error "__LIBCATAMOUNT__ not defined"
#endif
],
		[AC_CHECK_HEADERS([catamount/dclock.h],
		[AC_CHECK_HEADERS([catamount/data.h],
		[
			AC_DEFINE([TIMER_DCLOCK], [4], [Use `dclock' function])
			pform_timer=TIMER_DCLOCK
		])])])
		;;
	origin)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		pform_timer=TIMER_CLOCK_GETTIME
		;;
	generic)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		pform_timer=TIMER_GETTIMEOFDAY
		;;
	esac

	AC_DEFINE_UNQUOTED([TIMER], [$pform_timer], [Use timer (see below)])
	AC_MSG_NOTICE([selected timer: $pform_timer])

	AC_SUBST(PLATFORM)
])

AC_DEFUN([ACVT_ZLIB],
[
	zlib_error="no"
	check_zlib="yes"
        force_zlib="no"
	have_zlib="no"
	zlib_dir_withval=
	zlib_incdir_withval=
	zlib_libdir_withval=

	ZLIBDIR=
	ZLIBINCDIR=
	ZLIBLIBDIR=
	ZLIBLIB=

	AC_ARG_ENABLE(zlib,
		AC_HELP_STRING([--enable-zlib],
		[enable ZLIB trace compression support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_zlib="yes"], [check_zlib="no"])])

	AC_ARG_WITH(zlib-dir,
		AC_HELP_STRING([--with-zlib-dir=ZLIBDIR], [give the path for ZLIB, default: /usr]),
	[zlib_dir_withval=$withval; ZLIBDIR="$withval/"])

	AC_ARG_WITH(zlib-inc-dir,
		AC_HELP_STRING([--with-zlib-inc-dir=ZLIBINCDIR],
		[give the path for ZLIB-include files, default: ZLIB/include]),
	[zlib_incdir_withval=$withval; ZLIBINCDIR="-I$withval/"],
	[AS_IF([test x"$ZLIBDIR" != x], [ZLIBINCDIR="-I$ZLIBDIR"include/])])

	AC_ARG_WITH(zlib-lib-dir,
		AC_HELP_STRING([--with-zlib-lib-dir=ZLIBLIBDIR],
		[give the path for ZLIB-libraries, default: ZLIBDIR/lib]),
	[zlib_libdir_withval=$withval; ZLIBLIBDIR="-L$withval/"],
	[AS_IF([test x"$ZLIBDIR" != x], [ZLIBLIBDIR="-L$ZLIBDIR"lib/])])

	AC_ARG_WITH(zlib-lib,
		AC_HELP_STRING([--with-zlib-lib=ZLIBLIB], [use given zlib lib, default: -lz]),
	[ZLIBLIB="$withval"])

	AS_IF([test x"$check_zlib" = "xyes"],
        [
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $ZLIBINCDIR"
		AC_CHECK_HEADER([zlib.h], [],
		[
			AC_MSG_NOTICE([error: no zlib.h found; check path for ZLIB package first...])
			zlib_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$ZLIBLIB" = x -a x"$zlib_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $ZLIBLIBDIR -lz"
			AC_MSG_CHECKING([whether linking with -lz works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); ZLIBLIB=-lz],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$ZLIBLIB" = x -a x"$zlib_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libz found; check path for ZLIB package first...])
			zlib_error="yes"
		])

		AS_IF([test x"$ZLIBLIB" != x -a x"$zlib_error" = "xno"],
		[have_zlib="yes"])

		AS_IF([test x"$force_zlib" = "xyes" -a x"$zlib_error" = "xyes"],
		[exit 1])
	])

	AC_SUBST(ZLIBINCDIR)
	AC_SUBST(ZLIBLIBDIR)
	AC_SUBST(ZLIBLIB)
])

dnl @synopsis AX_OPENMP([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl @summary determine how to compile programs using OpenMP
dnl
dnl This macro tries to find out how to compile programs that use
dnl OpenMP a standard API and set of compiler directives for parallel
dnl programming (see http://www-unix.mcs/)
dnl
dnl On success, it sets the
dnl OPENMP_CFLAGS/OPENMP_CXXFLAGS/OPENMP_F77FLAGS output variable to
dnl the flag (e.g. -omp) used both to compile *and* link OpenMP
dnl programs in the current language.
dnl
dnl NOTE: You are assumed to not only compile your program with these
dnl flags, but also link it with them as well.
dnl
dnl If you want to compile everything with OpenMP, you should set:
dnl
dnl     CFLAGS="$CFLAGS $OPENMP_CFLAGS" 
dnl     #OR#  CXXFLAGS="$CXXFLAGS $OPENMP_CXXFLAGS" 
dnl     #OR#  FFLAGS="$FFLAGS $OPENMP_FFLAGS" 
dnl
dnl (depending on the selected language).
dnl
dnl The user can override the default choice by setting the
dnl corresponding environment variable (e.g. OPENMP_CFLAGS).
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if an OpenMP
dnl flag is found, and ACTION-IF-NOT-FOUND is a list of commands to run
dnl it if it is not found. If ACTION-IF-FOUND is not specified, the
dnl default action will define HAVE_OPENMP.
dnl
dnl @category InstalledPackages
dnl @author Steven G. Johnson <stevenj@alum.mit.edu>
dnl @version 2006-01-24
dnl @license GPLWithACException

AC_DEFUN([AX_OPENMP], [
AC_PREREQ(2.59) dnl for _AC_LANG_PREFIX

AC_CACHE_CHECK([for OpenMP flag of _AC_LANG compiler], ax_cv_[]_AC_LANG_ABBREV[]_openmp, [save[]_AC_LANG_PREFIX[]FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
ax_cv_[]_AC_LANG_ABBREV[]_openmp=unknown
# Flags to try:  -fopenmp (gcc), -openmp (icc), -mp (SGI & PGI),
#                -xopenmp (Sun), -omp (Tru64), -qsmp=omp (AIX), none
ax_openmp_flags="-mp -fopenmp -openmp -xopenmp -omp -qsmp=omp none"
if test "x$OPENMP_[]_AC_LANG_PREFIX[]FLAGS" != x; then
  ax_openmp_flags="$OPENMP_[]_AC_LANG_PREFIX[]FLAGS $ax_openmp_flags"
fi
for ax_openmp_flag in $ax_openmp_flags; do
  case $ax_openmp_flag in
    none) []_AC_LANG_PREFIX[]FLAGS=$save[]_AC_LANG_PREFIX[] ;;
    *) []_AC_LANG_PREFIX[]FLAGS="$save[]_AC_LANG_PREFIX[]FLAGS $ax_openmp_flag" ;;
  esac
  AC_TRY_LINK_FUNC(omp_set_num_threads,
	[ax_cv_[]_AC_LANG_ABBREV[]_openmp=$ax_openmp_flag; break])
done
[]_AC_LANG_PREFIX[]FLAGS=$save[]_AC_LANG_PREFIX[]FLAGS
])
if test "x$ax_cv_[]_AC_LANG_ABBREV[]_openmp" = "xunknown"; then
  m4_default([$2],:)
else
  if test "x$ax_cv_[]_AC_LANG_ABBREV[]_openmp" != "xnone"; then
    OPENMP_[]_AC_LANG_PREFIX[]FLAGS=$ax_cv_[]_AC_LANG_ABBREV[]_openmp
  fi
  m4_default([$1], [AC_DEFINE(HAVE_OPENMP,1,[Define if OpenMP is enabled])])
fi
])dnl AX_OPENMP

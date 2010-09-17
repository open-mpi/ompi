AC_DEFUN([ACVT_CONF_INIT],
[
	AC_ARG_ENABLE(config-summary,
		AC_HELP_STRING([--enable-config-summary],
			[show summary of configuration, default: enabled]), [],
	[
		AS_IF([test x"$inside_openmpi" = "xyes"],
		[enable_config_summary="no"], [enable_config_summary="yes"])
	])

	AC_ARG_ENABLE(config-titles,
		AC_HELP_STRING([--enable-config-titles],
			[show titles for each configure section, default: enabled]), [],
	[
		AS_IF([test x"$inside_openmpi" = "xyes"],
		[enable_config_titles="no"], [enable_config_titles="yes"])
	])
])

AC_DEFUN([ACVT_CONF_OPTIONS],
[
	options_dir="$srcdir/config/defaults"
	options_file=

	AC_REQUIRE([ACVT_PLATFORM])

	AC_MSG_CHECKING([for options file])

	AC_ARG_WITH(options,
		AC_HELP_STRING([--with-options=FILE],
		[load options from FILE]),
	[
		AC_MSG_RESULT([skipped (--with-options=$withval)])

		AS_IF([test x"$withval" = "xyes"],
		[AC_MSG_ERROR([value of '--with-options' not properly set])])

		AS_IF([test x"$withval" != "xno"],
		[
dnl			if no path, check in config/defaults
			AS_IF([test "`basename $withval`" = "$withval"],
			[
				AS_IF([test -r "$options_dir/$withval"],
				[withval="$options_dir/$withval"])
			])

dnl			make sure file exists
			AS_IF([test ! -r "$withval"],
			[AC_MSG_ERROR([options file '$withval' not found])])

			options_file="$withval"
		])
	],
	[
dnl		if no file given, generate options file name and look for it
dnl		in config/defaults
		AS_IF([test x"$inside_openmpi" = "xno"],
		[
			AS_IF([test x"$BITMODE" != x],
			[
				AS_IF([test -r "$options_dir/$PLATFORM-$BITMODE"],
				[options_file="$options_dir/$PLATFORM-$BITMODE"])
			])

			AS_IF([test x"$options_file" = x],
			[
				AS_IF([test -r "$options_dir/$PLATFORM"],
				[options_file="$options_dir/$PLATFORM"])
			])
		])

		AS_IF([test x"$options_file" != x],
		[AC_MSG_RESULT([$options_file])], [AC_MSG_RESULT([no])])
	])

dnl	load options from file
	AS_IF([test x"$options_file" != x],
	[
		AC_MSG_NOTICE([loading options from '$options_file'])

dnl		save command line options
		rm -f confopts
		for var in $ac_precious_vars; do
			set | grep ^$var= >>confopts
		done

		set | grep ^enable_.*= >>confopts
		set | grep ^with_.*= >>confopts

dnl		show + load options into environment
		cat $options_file
		set -a
		. $options_file
dnl		restore command line options
		. ./confopts
		set +a

		rm -f confopts
	])
])

AC_DEFUN([ACVT_CONF_TITLE],
[
	AS_IF([test x"$enable_config_titles" = "xyes"],
	[echo; echo "*** $1"])
])

AC_DEFUN([ACVT_CONF_SUBTITLE],
[
	AS_IF([test x"$enable_config_titles" = "xyes"],
	[echo "+++ $1"])
])

AC_DEFUN([ACVT_CONF_SUBSUBTITLE],
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
		AS_IF([test x"$options_file" != x],
                [answer="$options_file"], [answer="no"])
		echo "  Options file:                           $answer"
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
		echo ""

		AS_IF([test x"$have_mpi" = "xyes"],
		[
			echo "  MPI C compiler:                         $MPICC"
			echo "  MPI C++ compiler:                       $MPICXX"
			echo "  MPI Fortran 77 compiler:                $MPIF77"
			echo ""
			echo "  MPICFLAGS (append to CFLAGS):           $MPICFLAGS"
			echo "  MPICXXFLAGS (append to CXXFLAGS):       $MPICXXFLAGS"
			echo "  MPIFFLAGS (append to FFLAGS):           $MPIFFLAGS"
			echo ""
		])

		echo "  Source code location:                   $PWD"
		echo "  Install path:                           $prefix"
		echo ""
		answer=""
		AS_IF([test x"$use_extern_otf" = "xno"],
		[answer="yes"], [answer="no"])
		echo "  Build OTF library and tools:               $answer"

		AS_IF([test x"$have_zlib" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "   ZLIB trace compression support:           $answer"

		AS_IF([test x"$have_mpi" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build MPI support:                         $answer"

		AS_IF([test x"$have_mpi" = "xyes"],
		[
			AS_IF([test x"$have_fmpi" = "xyes"],
			[answer="yes"], [answer="no"])
			echo "   Build MPI Fortran support:                $answer"

			AS_IF([test x"$have_fmpi" = "xyes"],
			[
				AS_IF([test x"$build_fmpiwraplib" = "xyes"],
				[answer="yes"], [answer="no"])
				echo "    Build MPI Fortran wrapper library:       $answer"
			])

			echo "   Build MPI-2 support for"

			AS_IF([test x"$have_mpi2_1sided" = "xyes"],
			[answer="yes"], [answer="no"])
			echo "    One-Sided Communications:                $answer"

			AS_IF([test x"$have_mpi2_extcoll" = "xyes"],
			[answer="yes"], [answer="no"])
			echo "    Extended Collective Operations:          $answer"

			AS_IF([test x"$have_mpi2_io" = "xyes"],
			[answer="yes"], [answer="no"])
			echo "    I/O:                                     $answer"

			AS_IF([test x"$have_etimesync" = "xyes"],
			[answer="yes"], [answer="no"])
			echo "   Build enhanced time sync. support:        $answer"

			AS_IF([test x"$have_unimci" = "xyes"],
			[answer="yes ($unimci_checker_name $unimci_checker_version)"],
			[answer="no"])
			echo "   Build MPI correctness checking support:   $answer"
		])

		AS_IF([test x"$have_threads" = "xyes"],
		[
			answer=
			AS_IF([test x"$have_pthread" = "xyes"],
			[answer="POSIX threads"])
			AS_IF([test x"$have_omp" = "xyes"],
			[AS_IF([test x"$answer" != x],
			 [answer="$answer, OpenMP"],
			 [answer="OpenMP"])])
			answer="yes ($answer)"
		], [answer="no"])
		echo "  Build Multithreading support:              $answer"

		AS_IF([test x"$build_hybrid" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build Hybrid (MPI/Threads) support:        $answer"

		AS_IF([test x"$have_java" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build Java support:                        $answer"

		AS_IF([test x"$have_papi" = "xyes"],
		[answer="yes (PAPI)"], [AS_IF([test x"$have_cpc" = "xyes"],
		[answer="yes (CPC)"], [AS_IF([test x"$have_necsxcntr" = "xyes"],
		[answer="yes (NEC SX)"], [answer="no"])])])
		echo "  Build Hardware Perf. Counter support:      $answer"

		AS_IF([test x"$have_rusage" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build Resource usage trace support:        $answer"

		AS_IF([test x"$have_memhooks" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build GLIBC's memory alloc. trace support: $answer"

		AS_IF([test x"$have_getcpu" = "xyes"],
		[answer="yes"], [answer="no"])
		echo "  Build GLIBC's CPU ID trace support:        $answer"

		AS_IF([test x"$have_libwrap" = "xyes"],
		[
			answer="yes"
			AS_IF([test x"$have_libcwrap" = "xyes" -o x"$have_iowrap" = "xyes"],
			[
				answer=
				AS_IF([test x"$have_libcwrap" = "xyes"],
				[answer="LIBC"])
				AS_IF([test x"$have_iowrap" = "xyes"],
				[AS_IF([test x"$answer" != x],
				 [answer="$answer, LIBC-I/O"],
				 [answer="LIBC-I/O"])])
				answer="yes ($answer)"
			])
		], [answer="no"])
		echo "  Build Library trace support:               $answer"

		AS_IF([test x"$have_libwrap" = "xyes"],
		[
			AS_IF([test x"$build_libwrapgen" = "xyes"],
			[answer="yes"], [answer="no"])
			echo "   Build Library wrapper generator:          $answer"
		])

		AS_IF([test x"$compinst_type" != x],
		[answer=`echo $compinst_type | sed s/gnu/gnu*/g`],
		[answer="no"])
		echo ""
		echo "  Build compiler instrumentation support:    $answer"

		AS_IF([test x"$compinst_type" = "xgnu"],
		[
			echo ""
			echo "   * The instrumentation type 'gnu' also"
			echo "     works for Intel and Pathscale compilers."
		])

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

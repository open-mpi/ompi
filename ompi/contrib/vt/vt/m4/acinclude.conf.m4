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

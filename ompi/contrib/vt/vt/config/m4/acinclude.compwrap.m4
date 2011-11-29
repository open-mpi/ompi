AC_DEFUN([ACVT_COMPWRAP],
[
	VT_WRAPPER_CC_COMPILER=$CC
	VT_WRAPPER_CC_EXTRA_COMPILER_FLAGS=
	VT_WRAPPER_CC_EXTRA_LINKER_FLAGS=
	VT_WRAPPER_CC_EXTRA_LIBS=
	VT_WRAPPER_CC_DYNINST_COMPILER_FLAGS=
	VT_WRAPPER_CC_TAUINST_OPTS=
	VT_WRAPPER_CC_TAUINST_PARSE_BIN=
	VT_WRAPPER_CC_TAUINST_PARSE_OPTS=
	VT_WRAPPER_CC_COMPINST_COMPILER_FLAGS=
	VT_WRAPPER_CC_DEFAULT_PARTYPE="seq"

	VT_WRAPPER_CXX_COMPILER=$CXX
	VT_WRAPPER_CXX_EXTRA_COMPILER_FLAGS=
	VT_WRAPPER_CXX_EXTRA_LINKER_FLAGS=
	VT_WRAPPER_CXX_EXTRA_LIBS=
	VT_WRAPPER_CXX_DYNINST_COMPILER_FLAGS=
	VT_WRAPPER_CXX_TAUINST_OPTS=
	VT_WRAPPER_CXX_TAUINST_PARSE_BIN=
	VT_WRAPPER_CXX_TAUINST_PARSE_OPTS=
	VT_WRAPPER_CXX_COMPINST_COMPILER_FLAGS=
	VT_WRAPPER_CXX_DEFAULT_PARTYPE="seq"

	VT_WRAPPER_F77_COMPILER=$F77
	VT_WRAPPER_F77_EXTRA_COMPILER_FLAGS=
	VT_WRAPPER_F77_EXTRA_LINKER_FLAGS=
	VT_WRAPPER_F77_EXTRA_LIBS=
	VT_WRAPPER_F77_DYNINST_COMPILER_FLAGS=
	VT_WRAPPER_F77_TAUINST_OPTS=
	VT_WRAPPER_F77_TAUINST_PARSE_BIN=
	VT_WRAPPER_F77_TAUINST_PARSE_OPTS=
	VT_WRAPPER_F77_COMPINST_COMPILER_FLAGS=
	VT_WRAPPER_F77_DEFAULT_PARTYPE="seq"

	VT_WRAPPER_FC_COMPILER=$FC
	VT_WRAPPER_FC_EXTRA_COMPILER_FLAGS=
	VT_WRAPPER_FC_EXTRA_LINKER_FLAGS=
	VT_WRAPPER_FC_EXTRA_LIBS=
	VT_WRAPPER_FC_DYNINST_COMPILER_FLAGS=
	VT_WRAPPER_FC_TAUINST_OPTS=
	VT_WRAPPER_FC_TAUINST_PARSE_BIN=
	VT_WRAPPER_FC_TAUINST_PARSE_OPTS=
	VT_WRAPPER_FC_COMPINST_COMPILER_FLAGS=
	VT_WRAPPER_FC_DEFAULT_PARTYPE="seq"

	VT_WRAPPER_NVCC_COMPILER=$NVCC
	VT_WRAPPER_NVCC_EXTRA_COMPILER_FLAGS=
	VT_WRAPPER_NVCC_EXTRA_LINKER_FLAGS=
	VT_WRAPPER_NVCC_EXTRA_LIBS=
	VT_WRAPPER_NVCC_DYNINST_COMPILER_FLAGS=
	VT_WRAPPER_NVCC_TAUINST_OPTS=
	VT_WRAPPER_NVCC_TAUINST_PARSE_BIN=
	VT_WRAPPER_NVCC_TAUINST_PARSE_OPTS=
	VT_WRAPPER_NVCC_COMPINST_COMPILER_FLAGS=
	VT_WRAPPER_NVCC_DEFAULT_PARTYPE="seq"

	VT_WRAPPER_VTLIB="-lvt"
	VT_WRAPPER_VTMPILIB="-lvt-mpi"
	VT_WRAPPER_VTMTLIB="-lvt-mt"
	VT_WRAPPER_VTHYBLIB="-lvt-hyb"
	VT_WRAPPER_OPARI_BIN=
	VT_WRAPPER_OPARI_OPTS=
	VT_WRAPPER_OPARI_TAB_COMPILER=
	VT_WRAPPER_OPARI_TAB_COMPILER_FLAGS=
	VT_WRAPPER_TAUINST_BIN=
	VT_WRAPPER_AVAIL_INST="manual"
	VT_WRAPPER_DEFAULT_INST="manual"

	AC_REQUIRE([ACVT_PLATFORM])
	AC_REQUIRE([ACVT_COMPINST])
	AC_REQUIRE([ACVT_DYNINST])
	AC_REQUIRE([ACVT_THREADS])

	AC_ARG_WITH(wrapper-cc-compiler,
		AC_HELP_STRING([--with-wrapper-cc-compiler=WRAPPERCC],
		[underlying C compiler command for vtcc, default: CC]),
	[VT_WRAPPER_CC_COMPILER=$withval])

	AC_ARG_WITH(wrapper-cc-compiler-flags,
		AC_HELP_STRING([--with-wrapper-cc-compiler-flags=WRAPPERCCCFLAGS],
		[extra compiler flags to add when using vtcc]),
	[VT_WRAPPER_CC_EXTRA_COMPILER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-cc-linker-flags,
		AC_HELP_STRING([--with-wrapper-cc-linker-flags=WRAPPERCCLDFLAGS],
		[extra linker flags to add when using vtcc]),
	[VT_WRAPPER_CC_EXTRA_LINKER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-cc-libs,
		AC_HELP_STRING([--with-wrapper-cc-libs=WRAPPERCCLIBS],
		[extra libraries to link when using vtcc]),
	[VT_WRAPPER_CC_EXTRA_LIBS=$withval])

	AC_ARG_WITH(wrapper-cc-default-partype,
		AC_HELP_STRING([--with-wrapper-cc-default-partype=TYPE],
		[default parallelization type for vtcc (seq,mt,mpi,hyb), default: $VT_WRAPPER_CC_DEFAULT_PARTYPE]),
	[
		case $withval in
			seq | mt | mpi | hyb)
				VT_WRAPPER_CC_DEFAULT_PARTYPE=$withval
				;;
			*)
				AC_MSG_ERROR([value of '--with-wrapper-cc-default-partype' not properly set])
				;;
		esac
	])

	AC_ARG_WITH(wrapper-cxx-compiler,
		AC_HELP_STRING([--with-wrapper-cxx-compiler=WRAPPERCXX],
		[underlying C++ compiler command for vtcxx, default: CXX]),
	[VT_WRAPPER_CXX_COMPILER=$withval])

	AC_ARG_WITH(wrapper-cxx-compiler-flags,
		AC_HELP_STRING([--with-wrapper-cxx-compiler-flags=WRAPPERCXXCFLAGS],
		[extra compiler flags to add when using vtcxx]),
	[VT_WRAPPER_CXX_EXTRA_COMPILER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-cxx-linker-flags,
		AC_HELP_STRING([--with-wrapper-cxx-linker-flags=WRAPPERCXXLDFLAGS],
		[extra linker flags to add when using vtcxx]),
	[VT_WRAPPER_CXX_EXTRA_LINKER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-cxx-libs,
		AC_HELP_STRING([--with-wrapper-cxx-libs=WRAPPERCXXLIBS],
		[extra libraries to link when using vtcxx]),
	[VT_WRAPPER_CXX_EXTRA_LIBS=$withval])

	AC_ARG_WITH(wrapper-cxx-default-partype,
		AC_HELP_STRING([--with-wrapper-cxx-default-partype=TYPE],
		[default parallelization type for vtcxx (seq,mt,mpi,hyb), default: $VT_WRAPPER_CXX_DEFAULT_PARTYPE]),
	[
		case $withval in
			seq | mt | mpi | hyb)
				VT_WRAPPER_CXX_DEFAULT_PARTYPE=$withval
				;;
			*)
				AC_MSG_ERROR([value of '--with-wrapper-cxx-default-partype' not properly set])
				;;
		esac
	])

	AC_ARG_WITH(wrapper-f77-compiler,
		AC_HELP_STRING([--with-wrapper-f77-compiler=WRAPPERF77],
		[underlying Fortran 77 compiler command for vtf77, default: F77]),
	[VT_WRAPPER_F77_COMPILER=$withval])

	AC_ARG_WITH(wrapper-f77-compiler-flags,
		AC_HELP_STRING([--with-wrapper-f77-compiler-flags=WRAPPERF77CFLAGS],
		[extra compiler flags to add when using vtf77]),
	[VT_WRAPPER_F77_EXTRA_COMPILER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-f77-linker-flags,
		AC_HELP_STRING([--with-wrapper-f77-linker-flags=WRAPPERF77LDFLAGS],
		[extra linker flags to add when using vtf77]),
	[VT_WRAPPER_F77_EXTRA_LINKER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-f77-libs,
		AC_HELP_STRING([--with-wrapper-f77-libs=WRAPPERF77LIBS],
		[extra libraries to link when using vtf77]),
	[VT_WRAPPER_F77_EXTRA_LIBS=$withval])

	AC_ARG_WITH(wrapper-f77-default-partype,
		AC_HELP_STRING([--with-wrapper-f77-default-partype=TYPE],
		[default parallelization type for vtf77 (seq,mt,mpi,hyb), default: $VT_WRAPPER_F77_DEFAULT_PARTYPE]),
	[
		case $withval in
			seq | mt | mpi | hyb)
				VT_WRAPPER_F77_DEFAULT_PARTYPE=$withval
				;;
			*)
				AC_MSG_ERROR([value of '--with-wrapper-f77-default-partype' not properly set])
				;;
		esac
	])

	AC_ARG_WITH(wrapper-fc-compiler,
		AC_HELP_STRING([--with-wrapper-fc-compiler=WRAPPERFC],
		[underlying Fortran compiler command for vtf90, default: FC]),
	[VT_WRAPPER_FC_COMPILER=$withval])

	AC_ARG_WITH(wrapper-fc-compiler-flags,
		AC_HELP_STRING([--with-wrapper-fc-compiler-flags=WRAPPERFCCFLAGS],
		[extra compiler flags to add when using vtf90]),
	[VT_WRAPPER_FC_EXTRA_COMPILER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-fc-linker-flags,
		AC_HELP_STRING([--with-wrapper-fc-linker-flags=WRAPPERFCLDFLAGS],
		[extra linker flags to add when using vtf90]),
	[VT_WRAPPER_FC_EXTRA_LINKER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-fc-libs,
		AC_HELP_STRING([--with-wrapper-fc-libs=WRAPPERFCLIBS],
		[extra libraries to link when using vtf90]),
	[VT_WRAPPER_FC_EXTRA_LIBS=$withval])

	AC_ARG_WITH(wrapper-fc-default-partype,
		AC_HELP_STRING([--with-wrapper-fc-default-partype=TYPE],
		[default parallelization type for vtf90 (seq,mt,mpi,hyb), default: $VT_WRAPPER_FC_DEFAULT_PARTYPE]),
	[
		case $withval in
			seq | mt | mpi | hyb)
				VT_WRAPPER_FC_DEFAULT_PARTYPE=$withval
				;;
			*)
				AC_MSG_ERROR([value of '--with-wrapper-fc-default-partype' not properly set])
				;;
		esac
	])

	AC_ARG_WITH(wrapper-nvcc-compiler,
		AC_HELP_STRING([--with-wrapper-nvcc-compiler=WRAPPERNVCC],
		[underlying NVIDIA CUDA compiler command for vtnvcc, default: NVCC]),
	[VT_WRAPPER_NVCC_COMPILER=$withval])

	AC_ARG_WITH(wrapper-nvcc-compiler-flags,
		AC_HELP_STRING([--with-wrapper-nvcc-compiler-flags=WRAPPERNVCCCFLAGS],
		[extra compiler flags to add when using vtnvcc]),
	[VT_WRAPPER_NVCC_EXTRA_COMPILER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-nvcc-linker-flags,
		AC_HELP_STRING([--with-wrapper-nvcc-linker-flags=WRAPPERNVCCLDFLAGS],
		[extra linker flags to add when using vtnvcc]),
	[VT_WRAPPER_NVCC_EXTRA_LINKER_FLAGS=$withval])

	AC_ARG_WITH(wrapper-nvcc-libs,
		AC_HELP_STRING([--with-wrapper-nvcc-libs=WRAPPERNVCCLIBS],
		[extra libraries to link when using vtnvcc]),
	[VT_WRAPPER_NVCC_EXTRA_LIBS=$withval])

	AC_ARG_WITH(wrapper-nvcc-default-partype,
		AC_HELP_STRING([--with-wrapper-nvcc-default-partype=TYPE],
		[default parallelization type for vtnvcc (seq,mt,mpi,hyb), default: $VT_WRAPPER_NVCC_DEFAULT_PARTYPE]),
	[
		case $withval in
			seq | mt | mpi | hyb)
				VT_WRAPPER_NVCC_DEFAULT_PARTYPE=$withval
				;;
			*)
				AC_MSG_ERROR([value of '--with-wrapper-nvcc-default-partype' not properly set])
				;;
		esac
	])

	AS_IF([test "$PLATFORM" = "bgp" -a x"$enable_shared" = "xyes"],
	[
		VT_WRAPPER_CC_EXTRA_LINKER_FLAGS="$VT_WRAPPER_CC_EXTRA_LINKER_FLAGS -Wl,-dy"
		VT_WRAPPER_CXX_EXTRA_LINKER_FLAGS="$VT_WRAPPER_CXX_EXTRA_LINKER_FLAGS -Wl,-dy"
		VT_WRAPPER_F77_EXTRA_LINKER_FLAGS="$VT_WRAPPER_F77_EXTRA_LINKER_FLAGS -Wl,-dy"
		VT_WRAPPER_FC_EXTRA_LINKER_FLAGS="$VT_WRAPPER_FC_EXTRA_LINKER_FLAGS -Wl,-dy"
		VT_WRAPPER_NVCC_EXTRA_LINKER_FLAGS="$VT_WRAPPER_NVCC_EXTRA_LINKER_FLAGS -Wl,-dy"
	])

	AS_IF([test x"$compinst_type" = "xpgi9"],
	[
		VT_WRAPPER_VTLIB="-Wl,--whole-archive $VT_WRAPPER_VTLIB -Wl,--no-whole-archive"
		VT_WRAPPER_VTMPILIB="-Wl,--whole-archive $VT_WRAPPER_VTMPILIB -Wl,--no-whole-archive"
		VT_WRAPPER_VTMTLIB="-Wl,--whole-archive $VT_WRAPPER_VTMTLIB -Wl,--no-whole-archive"
		VT_WRAPPER_VTHYBLIB="-Wl,--whole-archive $VT_WRAPPER_VTHYBLIB -Wl,--no-whole-archive"
	])

	AS_IF([test x"$build_opari" = "xyes"],
	[
		VT_WRAPPER_OPARI_BIN="\${bindir}/opari"

		for f in -V --version; do
			case `$CC $f 2>&1` in
				*pgcc\ [[1-8]].* | *PathScale*)
					VT_WRAPPER_OPARI_OPTS="-nodecl"
					break
					;;
			esac
		done

		VT_WRAPPER_OPARI_TAB_COMPILER="$CC"
		VT_WRAPPER_OPARI_TAB_COMPILER_FLAGS="$CFLAGS"
	])

	AS_IF([test x"$have_compinst" = "xyes"],
	[
		VT_WRAPPER_CC_COMPINST_COMPILER_FLAGS="$compinst_cflags"
		VT_WRAPPER_CXX_COMPINST_COMPILER_FLAGS="$compinst_cxxflags"
		VT_WRAPPER_F77_COMPINST_COMPILER_FLAGS="$compinst_fflags"
		VT_WRAPPER_FC_COMPINST_COMPILER_FLAGS="$compinst_fcflags"
		VT_WRAPPER_NVCC_COMPINST_COMPILER_FLAGS="-Xcompiler=\"$compinst_cflags\""
		VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST compinst"
		VT_WRAPPER_DEFAULT_INST="compinst"
	])

	AS_IF([test x"$have_dyninst" = "xyes"],
	[
		VT_WRAPPER_CC_DYNINST_COMPILER_FLAGS="-g"
		VT_WRAPPER_CXX_DYNINST_COMPILER_FLAGS="-g"
		VT_WRAPPER_F77_DYNINST_COMPILER_FLAGS="-g"
		VT_WRAPPER_FC_DYNINST_COMPILER_FLAGS="-g"
		VT_WRAPPER_NVCC_DYNINST_COMPILER_FLAGS="-Xcompiler=\"-g\""
		VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST dyninst"
	])

	AS_IF([test x"$have_tauinst" = "xyes"],
	[
		pdt_mpiincdir=
		pdt_fmpiincdir=

		AS_IF([test x"$have_mpi" = "xyes"],
		[
			AS_IF([test x"$inside_openmpi" = "xyes"],
			[pdt_mpiincdir="-I\${includedir}/.."],
			[pdt_mpiincdir="$MPIINCDIR"])
			AS_IF([test x"$have_fmpi" = "xyes"],
			[
				AS_IF([test x"$inside_openmpi" = "xyes"],
				[pdt_fmpiincdir="$pdt_mpiincdir"],
				[pdt_fmpiincdir="$FMPIINCDIR"])
			])
		])

		VT_WRAPPER_TAUINST_BIN="$tauinst_cmd"
		VT_WRAPPER_CC_TAUINST_OPTS="-c -spec \${datadir}/TAUINST.SPEC"
		VT_WRAPPER_CC_TAUINST_PARSE_BIN="$tauinst_cparse_cmd"
		VT_WRAPPER_CC_TAUINST_PARSE_OPTS="$pdt_mpiincdir"
		VT_WRAPPER_CXX_TAUINST_OPTS="-c++ -spec \${datadir}/TAUINST.SPEC"
		VT_WRAPPER_CXX_TAUINST_PARSE_BIN="$tauinst_cxxparse_cmd"
		VT_WRAPPER_CXX_TAUINST_PARSE_OPTS="$VT_WRAPPER_CC_TAUINST_PARSE_OPTS"
		VT_WRAPPER_F77_TAUINST_OPTS="-fortran -spec \${datadir}/TAUINST.SPEC"
		VT_WRAPPER_F77_TAUINST_PARSE_BIN="$tauinst_fparse_cmd"
		VT_WRAPPER_F77_TAUINST_PARSE_OPTS="$pdt_fmpiincdir"
		VT_WRAPPER_FC_TAUINST_OPTS="$VT_WRAPPER_F77_TAUINST_OPTS"
		VT_WRAPPER_FC_TAUINST_PARSE_BIN="$VT_WRAPPER_F77_TAUINST_PARSE_BIN"
		VT_WRAPPER_FC_TAUINST_PARSE_OPTS="$VT_WRAPPER_F77_TAUINST_PARSE_OPTS"
		VT_WRAPPER_NVCC_TAUINST_OPTS="$VT_WRAPPER_CC_TAUINST_OPTS"
		VT_WRAPPER_NVCC_TAUINST_PARSE_BIN="$VT_WRAPPER_CC_TAUINST_PARSE_BIN"
		VT_WRAPPER_NVCC_TAUINST_PARSE_OPTS="$VT_WRAPPER_CC_TAUINST_PARSE_OPTS"
		VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST tauinst"
	])

	AC_SUBST(VT_WRAPPER_CC_COMPILER)
	AC_SUBST(VT_WRAPPER_CC_EXTRA_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_CC_EXTRA_LINKER_FLAGS)
	AC_SUBST(VT_WRAPPER_CC_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_CC_DYNINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_CC_TAUINST_OPTS)
	AC_SUBST(VT_WRAPPER_CC_TAUINST_PARSE_BIN)
	AC_SUBST(VT_WRAPPER_CC_TAUINST_PARSE_OPTS)
	AC_SUBST(VT_WRAPPER_CC_COMPINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_CC_DEFAULT_PARTYPE)

	AC_SUBST(VT_WRAPPER_CXX_COMPILER)
	AC_SUBST(VT_WRAPPER_CXX_EXTRA_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_CXX_EXTRA_LINKER_FLAGS)
	AC_SUBST(VT_WRAPPER_CXX_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_CXX_DYNINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_CXX_TAUINST_OPTS)
	AC_SUBST(VT_WRAPPER_CXX_TAUINST_PARSE_BIN)
	AC_SUBST(VT_WRAPPER_CXX_TAUINST_PARSE_OPTS)
	AC_SUBST(VT_WRAPPER_CXX_COMPINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_CXX_DEFAULT_PARTYPE)

	AC_SUBST(VT_WRAPPER_F77_COMPILER)
	AC_SUBST(VT_WRAPPER_F77_EXTRA_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_F77_EXTRA_LINKER_FLAGS)
	AC_SUBST(VT_WRAPPER_F77_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_F77_DYNINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_F77_TAUINST_OPTS)
	AC_SUBST(VT_WRAPPER_F77_TAUINST_PARSE_BIN)
	AC_SUBST(VT_WRAPPER_F77_TAUINST_PARSE_OPTS)
	AC_SUBST(VT_WRAPPER_F77_COMPINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_F77_DEFAULT_PARTYPE)

	AC_SUBST(VT_WRAPPER_FC_COMPILER)
	AC_SUBST(VT_WRAPPER_FC_EXTRA_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_FC_EXTRA_LINKER_FLAGS)
	AC_SUBST(VT_WRAPPER_FC_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_FC_DYNINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_FC_TAUINST_OPTS)
	AC_SUBST(VT_WRAPPER_FC_TAUINST_PARSE_BIN)
	AC_SUBST(VT_WRAPPER_FC_TAUINST_PARSE_OPTS)
	AC_SUBST(VT_WRAPPER_FC_COMPINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_FC_DEFAULT_PARTYPE)

	AC_SUBST(VT_WRAPPER_NVCC_COMPILER)
	AC_SUBST(VT_WRAPPER_NVCC_EXTRA_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_NVCC_EXTRA_LINKER_FLAGS)
	AC_SUBST(VT_WRAPPER_NVCC_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_NVCC_DYNINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_NVCC_TAUINST_OPTS)
	AC_SUBST(VT_WRAPPER_NVCC_TAUINST_PARSE_BIN)
	AC_SUBST(VT_WRAPPER_NVCC_TAUINST_PARSE_OPTS)
	AC_SUBST(VT_WRAPPER_NVCC_COMPINST_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_NVCC_DEFAULT_PARTYPE)

	AC_SUBST(VT_WRAPPER_VTLIB)
	AC_SUBST(VT_WRAPPER_VTMPILIB)
	AC_SUBST(VT_WRAPPER_VTMTLIB)
	AC_SUBST(VT_WRAPPER_VTHYBLIB)
	AC_SUBST(VT_WRAPPER_OPARI_BIN)
	AC_SUBST(VT_WRAPPER_OPARI_OPTS)
	AC_SUBST(VT_WRAPPER_OPARI_TAB_COMPILER)
	AC_SUBST(VT_WRAPPER_OPARI_TAB_COMPILER_FLAGS)
	AC_SUBST(VT_WRAPPER_TAUINST_BIN)
	AC_SUBST(VT_WRAPPER_AVAIL_INST)
	AC_SUBST(VT_WRAPPER_DEFAULT_INST)
])


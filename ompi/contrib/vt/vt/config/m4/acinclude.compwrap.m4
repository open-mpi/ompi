AC_DEFUN([ACVT_COMPWRAP],
[
	VT_WRAPPER_CC=
	VT_WRAPPER_CXX=
	VT_WRAPPER_F77=
	VT_WRAPPER_FC=
	VT_WRAPPER_EXTRA_CFLAGS=
	VT_WRAPPER_EXTRA_CXXFLAGS=
	VT_WRAPPER_EXTRA_FFLAGS=
	VT_WRAPPER_EXTRA_FCFLAGS=
	VT_WRAPPER_EXTRA_LDFLAGS=
	VT_WRAPPER_EXTRA_LIBS=
	VT_WRAPPER_VTLIB=
	VT_WRAPPER_VTMPILIB=
	VT_WRAPPER_VTMTLIB=
	VT_WRAPPER_VTHYBLIB=
	VT_WRAPPER_OPARI_BIN=
	VT_WRAPPER_OPARI_TAB_CC=
	VT_WRAPPER_OPARI_TAB_CFLAGS=
	VT_WRAPPER_COMPINST_CFLAGS=
	VT_WRAPPER_COMPINST_CXXFLAGS=
	VT_WRAPPER_COMPINST_FFLAGS=
	VT_WRAPPER_COMPINST_FCFLAGS=
	VT_WRAPPER_AVAIL_INST="manual"
	VT_WRAPPER_DEFAULT_INST=manual

	AC_REQUIRE([ACVT_PLATFORM])
	AC_REQUIRE([ACVT_COMPINST])
	AC_REQUIRE([ACVT_DYNINST])
	AC_REQUIRE([ACVT_THREADS])

	AC_ARG_WITH(wrapper-cc,
		AC_HELP_STRING([--with-wrapper-cc],
		[underlying C compiler command for vtcc, default: CC]),
	[VT_WRAPPER_CC=$withval], [VT_WRAPPER_CC=$CC])

	AC_ARG_WITH(wrapper-cflags,
		AC_HELP_STRING([--with-wrapper-cflags],
		[extra flags to add to CFLAGS when using vtcc]),
	[VT_WRAPPER_EXTRA_CFLAGS=$withval])

	AC_ARG_WITH(wrapper-cxx,
		AC_HELP_STRING([--with-wrapper-cxx],
		[underlying C++ compiler command for vtcxx, default: CXX]),
	[VT_WRAPPER_CXX=$withval], [VT_WRAPPER_CXX=$CXX])

	AC_ARG_WITH(wrapper-cxxflags,
		AC_HELP_STRING([--with-wrapper-cxxflags],
		[extra flags to add to CXXFLAGS when using vtcxx]),
	[VT_WRAPPER_EXTRA_CXXFLAGS=$withval])

	AC_ARG_WITH(wrapper-f77,
		AC_HELP_STRING([--with-wrapper-f77],
		[underlying Fortran 77 compiler command for vtf77, default: F77]),
	[VT_WRAPPER_F77=$withval], [VT_WRAPPER_F77=$F77])

	AC_ARG_WITH(wrapper-fflags,
		AC_HELP_STRING([--with-wrapper-fflags],
		[extra flags to add to FFLAGS when using vtf77]),
	[VT_WRAPPER_EXTRA_FFLAGS=$withval])

	AC_ARG_WITH(wrapper-fc,
		AC_HELP_STRING([--with-wrapper-fc],
		[underlying Fortran compiler command for vtf90, default: FC]),
	[VT_WRAPPER_FC=$withval], [VT_WRAPPER_FC=$FC])

	AC_ARG_WITH(wrapper-fcflags,
		AC_HELP_STRING([--with-wrapper-fcflags],
		[extra flags to add to FCFLAGS when using vtf90]),
	[VT_WRAPPER_EXTRA_FCFLAGS=$withval])

	AC_ARG_WITH(wrapper-ldflags,
		AC_HELP_STRING([--with-wrapper-ldflags],
		[extra flags to add to LDFLAGS when using wrapper compilers]),
	[VT_WRAPPER_EXTRA_LDFLAGS=$withval])

	AC_ARG_WITH(wrapper-libs,
		AC_HELP_STRING([--with-wrapper-libs],
		[extra flags to add to LIBS when using wrapper compilers]),
	[VT_WRAPPER_EXTRA_LIBS=$withval])

	AS_IF([test "$PLATFORM" = "bgp" -a x"$enable_shared" = "xyes"],
	[
		VT_WRAPPER_EXTRA_LDFLAGS="$VT_WRAPPER_EXTRA_LDFLAGS -Wl,-dy"
	])

	VT_WRAPPER_VTLIB="-lvt"
	VT_WRAPPER_VTMPILIB="-lvt-mpi"
	VT_WRAPPER_VTMTLIB="-lvt-mt"
	VT_WRAPPER_VTHYBLIB="-lvt-hyb"

	AS_IF([test x"$compinst_type" = "xpgi9"],
	[
		VT_WRAPPER_VTLIB="-Wl,--whole-archive $VT_WRAPPER_VTLIB -Wl,--no-whole-archive"
		VT_WRAPPER_VTMPILIB="-Wl,--whole-archive $VT_WRAPPER_VTMPILIB -Wl,--no-whole-archive"
		VT_WRAPPER_VTMTLIB="-Wl,--whole-archive $VT_WRAPPER_VTMTLIB -Wl,--no-whole-archive"
		VT_WRAPPER_VTHYBLIB="-Wl,--whole-archive $VT_WRAPPER_VTHYBLIB -Wl,--no-whole-archive"
	])

	AS_IF([test x"$build_opari" = "xyes"],
	[
		ACVT_CONF_EXPAND_VARS([$bindir/opari], [VT_WRAPPER_OPARI_BIN])
		VT_WRAPPER_OPARI_TAB_CC="$CC"
		VT_WRAPPER_OPARI_TAB_CFLAGS="$CFLAGS"
	])

	AS_IF([test x"$compinst_type" != x],
	[
		VT_WRAPPER_COMPINST_CFLAGS="$compinst_cflags"
		VT_WRAPPER_COMPINST_CXXFLAGS="$compinst_cxxflags"
		VT_WRAPPER_COMPINST_FFLAGS="$compinst_fflags"
		VT_WRAPPER_COMPINST_FCFLAGS="$compinst_fcflags"
		VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST compinst"
		VT_WRAPPER_DEFAULT_INST="compinst"
	])

	AS_IF([test x"$have_dyninst" = "xyes"],
	[
		VT_WRAPPER_AVAIL_INST="$VT_WRAPPER_AVAIL_INST dyninst"
	])

	AC_SUBST(VT_WRAPPER_CC)
	AC_SUBST(VT_WRAPPER_CXX)
	AC_SUBST(VT_WRAPPER_F77)
	AC_SUBST(VT_WRAPPER_FC)
	AC_SUBST(VT_WRAPPER_EXTRA_CFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_CXXFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_FFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_FCFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_LDFLAGS)
	AC_SUBST(VT_WRAPPER_EXTRA_LIBS)
	AC_SUBST(VT_WRAPPER_VTLIB)
	AC_SUBST(VT_WRAPPER_VTMPILIB)
	AC_SUBST(VT_WRAPPER_VTMTLIB)
	AC_SUBST(VT_WRAPPER_VTHYBLIB)
	AC_SUBST(VT_WRAPPER_OPARI_BIN)
	AC_SUBST(VT_WRAPPER_OPARI_TAB_CC)
	AC_SUBST(VT_WRAPPER_OPARI_TAB_CFLAGS)
	AC_SUBST(VT_WRAPPER_COMPINST_CFLAGS)
        AC_SUBST(VT_WRAPPER_COMPINST_CXXFLAGS)
        AC_SUBST(VT_WRAPPER_COMPINST_FFLAGS)
        AC_SUBST(VT_WRAPPER_COMPINST_FCFLAGS)
	AC_SUBST(VT_WRAPPER_AVAIL_INST)
	AC_SUBST(VT_WRAPPER_DEFAULT_INST)
])


AC_DEFUN([ACVT_COMPWRAP],
[
	VT_WRAPPER_EXTRA_CFLAGS=
	VT_WRAPPER_EXTRA_CXXFLAGS=
	VT_WRAPPER_EXTRA_FFLAGS=
	VT_WRAPPER_EXTRA_FCFLAGS=
	VT_WRAPPER_EXTRA_LDFLAGS=
	VT_WRAPPER_EXTRA_LIBS=
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
	AC_SUBST(VT_WRAPPER_OPARI_BIN)
	AC_SUBST(VT_WRAPPER_AVAIL_INST)
	AC_SUBST(VT_WRAPPER_DEFAULT_INST)
])


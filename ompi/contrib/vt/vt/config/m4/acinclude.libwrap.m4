AC_DEFUN([ACVT_LIBWRAP],
[
	libwrap_error="no"
	check_libwrap="gen libc io"
	force_libwrap="no"
	have_libwrap="no"

	force_libwrapgen="no"
	build_libwrapgen="no"
	force_libcwrap="no"
	force_iowrap="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(libtrace,
		AC_HELP_STRING([--enable-libtrace=LIST],
			[enable library tracing support (gen,libc,io), default: automatically by configure]),
	[
		AS_IF([test x"$enableval" = "xno"], [check_libwrap="no"])
		AS_IF([test x"$enableval" = "xyes"], [force_libwrap="yes"])
		AS_IF([test x"$enableval" != "xyes" -a x"$enableval" != "xno"],
		[
			check_libwrap=`echo $enableval | sed 's/,/ /g'`
			for lw in $check_libwrap
			do
				case $lw in
					gen)
						force_libwrapgen="yes"
						;;
					libc)
						force_libcwrap="yes"
						;;
					io)
						force_iowrap="yes"
						;;
					*)
						AC_MSG_ERROR([value of '--enable-libtrace' not properly set])
						;;
				esac
			done
			force_libwrap="yes"
		])
	])

	AS_IF([test x"$check_libwrap" != "xno"],
	[
		AS_IF([test "$PLATFORM" = "bgp" -a x"$enable_shared" = "xno"],
		[
			AC_MSG_NOTICE([error: library tracing requires building of shared libraries on this platform; re-configure with \`--enable-shared'])
			libwrap_error="yes"
		])

		AS_IF([test x"$libwrap_error" = "xno"],
		[
			ACVT_DL
			AS_IF([test x"$have_dl" = "xno"], [libwrap_error="yes"])
		])

		AS_IF([test x"$libwrap_error" = "xno"],
		[
			for lw in $check_libwrap
			do
				case $lw in
				gen)
					ACVT_CONF_SUBTITLE([Library wrapper generator])
					ACVT_CTOOL
					AS_IF([test x"$have_ctool" = "xyes"],
					[have_libwrap="yes"; build_libwrapgen="yes"],
					[
						AS_IF([test x"$force_libwrapgen" = "xyes"],
						[libwrap_error="yes"; break])
					])
					;;
				libc)
					ACVT_CONF_SUBTITLE([LIBC])
					ACVT_LIBCWRAP
					AS_IF([test x"$have_libcwrap" = "xyes"], [have_libwrap="yes"],
					[
						AS_IF([test x"$force_libcwrap" = "xyes"],
						[libwrap_error="yes"; break])
					])
					;;
				io)
					ACVT_CONF_SUBTITLE([LIBC-I/O])
					ACVT_IOWRAP
					AS_IF([test x"$have_iowrap" = "xyes"], [have_libwrap="yes"],
					[
						AS_IF([test x"$force_iowrap" = "xyes"],
						[libwrap_error="yes"; break])
					])
					;;
				esac
			done
		])
	])
])


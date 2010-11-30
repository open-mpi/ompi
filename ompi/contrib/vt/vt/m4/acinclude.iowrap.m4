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
				__fprintf_chk \
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


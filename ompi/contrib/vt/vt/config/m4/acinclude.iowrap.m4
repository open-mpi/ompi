AC_DEFUN([ACVT_IOWRAP],
[
	iowrap_error="no"
	have_iowrap="no"

	AS_IF([test x"$libc_error" = x], [ACVT_LIBC])
	AS_IF([test x"$libc_pathname" = x], [iowrap_error="yes"])

	AS_IF([test x"$iowrap_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE -D_LARGEFILE64_SOURCE"
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
			pwrite64 \
			flockfile \
			ftrylockfile \
			funlockfile \
			sync \
			fflush \
			fsync \
			fdatasync \
			lockf])
		CPPFLAGS=$sav_CPPFLAGS

		have_iowrap="yes"
	])
])


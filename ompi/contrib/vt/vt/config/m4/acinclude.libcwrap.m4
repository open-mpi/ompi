AC_DEFUN([ACVT_LIBCWRAP],
[
	libcwrap_error="no"
	have_libcwrap="no"
	have_fork="no"

	AS_IF([test x"$libc_error" = x], [ACVT_LIBC])
	AS_IF([test x"$libc_pathname" = x], [libcwrap_error="yes"])

	AS_IF([test x"$libcwrap_error" = "xno"],
	[
		AC_CHECK_FUNCS([fork], [have_fork="yes"])

		AC_CHECK_FUNCS([ \
			execl \
			execle \
			execlp \
			execv \
			execve \
			execvp \
			system \
			wait \
			waitpid])

		have_libcwrap="yes"
	])
])


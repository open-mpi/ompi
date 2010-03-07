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


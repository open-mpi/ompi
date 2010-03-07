AC_DEFUN([ACVT_LTMPDIR],
[
	AC_ARG_WITH(local-tmp-dir,
                AC_HELP_STRING([--with-local-tmp-dir=LTMPDIR],
			[give the path for node-local temporary directory, default: /tmp]),
		[LTMPDIR="$withval"], [LTMPDIR=""])

	AS_IF([test x"$LTMPDIR" != x],
	[AC_DEFINE_UNQUOTED(PFORM_LDIR, ["$LTMPDIR"], [Path for node-local temporary directory])])
])


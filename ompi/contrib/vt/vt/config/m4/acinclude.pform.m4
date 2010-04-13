AC_DEFUN([ACVT_PLATFORM],
[
	PLATFORM=
	BITMODE=

	AC_CANONICAL_HOST

	AC_MSG_CHECKING([for platform])

	AC_ARG_WITH(platform,
		AC_HELP_STRING([--with-platform=PLATFORM],
		[configure for given platform (altix,bgl,bgp,crayt3e,crayx1,crayxt,ibm,linux,macos,necsx,origin,sicortex,sun,generic), default: automatically by configure]),
	[
		AC_MSG_RESULT([skipped (--with-platform=$withval)])

		pform_list="altix bgl bgp crayt3e crayx1 crayxt ibm linux macos necsx origin sicortex sun generic"
		pform_found="no"
		for p in $pform_list
		do
			AS_IF([test x"$withval" = x"$p"],
			[pform_found="yes"; break])
		done

		AS_IF([test x"$pform_found" = "xno"],
		[AC_MSG_ERROR([value of '--with-platform' not properly set])],
		[PLATFORM=$withval])
	],
	[
		case $host_os in
			linux*)
				AS_IF([test "$host_cpu" = "ia64" -a -f /etc/sgi-release],
				[PLATFORM=altix],
				[AS_IF([test "$host_cpu" = "powerpc64" -a -d /bgl/BlueLight],
				 [PLATFORM=bgl],
				 [AS_IF([test "$host_cpu" = "powerpc64" -a -d /bgsys],
				  [PLATFORM=bgp],
				  [AS_IF([test "$host_cpu" = "x86_64" -a -d /opt/xt-boot],
				   [PLATFORM=crayxt],
				   [AS_IF([test "$host_cpu" = "mips64" -a -d /opt/sicortex],
				    [PLATFORM=sicortex],
				    [PLATFORM=linux])])])])])
				;;
			sunos* | solaris*)
				PLATFORM=sun
				;;
			darwin*)
				PLATFORM=macos
				;;
			irix*)
				AS_IF([test "$host_cpu" = "mips"], [PLATFORM="origin"])
				;;
			aix*)
				PLATFORM=ibm
				;;
			unicosmp*)
				PLATFORM=crayx1
				;;
			superux*)
				PLATFORM=necsx
				;;
		esac

		AS_IF([test x"$PLATFORM" = x],
		[
			AC_MSG_WARN([unknown platform '$host'! using generic configuration])
			PLATFORM=generic
		],
		[
			AC_MSG_RESULT([$PLATFORM])
		])
	])

	AC_ARG_WITH(bitmode,
		AC_HELP_STRING([--with-bitmode=<32|64>],
		[specify bit mode]),
	[
		AS_IF([test x"$withval" != "x32" -a x"$withval" != "x64"],
		[AC_MSG_ERROR([value of '--with-bitmode' not properly set])])
		BITMODE=$withval
        ])


	AS_IF([test "$PLATFORM" = "bgp"],
	[
		CPPFLAGS="$CPPFLAGS -I/bgsys/drivers/ppcfloor/arch/include"
	])

	AC_SUBST(PLATFORM)
	AC_SUBST(BITMODE)
])


AC_DEFUN([ACVT_OTF],
[
	otf_error="no"

	OTFDIR=
	OTFINCDIR=
	OTFLIBDIR=

	AC_REQUIRE([ACVT_MPI])
	AC_REQUIRE([ACVT_ZLIB])

	AC_ARG_WITH(extern-otf,
		AC_HELP_STRING([--with-extern-otf], [use external OTF library, default: not set]),
	[use_extern_otf="yes"], [use_extern_otf="no"])

	AC_ARG_WITH(extern-otf-dir,
		AC_HELP_STRING([--with-extern-otf-dir=OTFDIR], [give the path for OTF, default: /usr]),
	[OTFDIR="$withval/"])

	AC_ARG_WITH(extern-otf-inc-dir,
		AC_HELP_STRING([--with-extern-otf-inc-dir=OTFINCDIR],
		[give the path for OTF-include files, default: OTFDIR/include]),
	[OTFINCDIR="-I$withval/"],
	[AS_IF([test x"$OTFDIR" != x], [OTFINCDIR="-I$OTFDIR"include/])])

	AC_ARG_WITH(extern-otf-lib-dir,
		AC_HELP_STRING([--with-extern-otf-lib-dir=OTFLIBDIR],
		[give the path for OTF-libraries, default: OTFDIR/lib]),
	[OTFLIBDIR="-L$withval/"],
	[AS_IF([test x"$OTFDIR" != x], [OTFLIBDIR="-L$OTFDIR"lib/])])

	AC_ARG_WITH(otf-lib,
		AC_HELP_STRING([--with-otf-lib=OTFLIB], [use given otf lib, default: -lotf ZLIBLIBDIR ZLIBLIB]),
	[OTFLIB="$withval"])

	AC_ARG_WITH(otf-flags,
		AC_HELP_STRING([--with-otf-flags=FLAGS], [pass FLAGS to the OTF distribution configuration script]), [OTFFLAGS="$withval"])

	AS_IF([test "$use_extern_otf" = "yes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $OTFINCDIR"
		AC_CHECK_HEADER([otf.h], [],
		[
			AC_MSG_NOTICE([error: no otf.h found; check path for OTF package first...])
			otf_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$OTFLIB" = x -a "$otf_error" = "no"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $OTFLIBDIR -lotf $ZLIBLIBDIR $ZLIBLIB"
			AC_MSG_CHECKING([whether linking with -lotf $ZLIBLIBDIR $ZLIBLIB works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); OTFLIB="-lotf $ZLIBLIBDIR $ZLIBLIB"],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$OTFLIB" = x -a "$otf_error" = "no"],
		[
			AC_MSG_NOTICE([error: no libotf found; check path for OTF package first...])
			otf_error="yes"
		])
	],
	[
		otf_parent_dir=`pwd`
		otf_dir="extlib/otf"

		AC_MSG_NOTICE([configuring in $otf_dir ($otf_parent_dir/$otf_dir)])

		AS_IF([test "$srcdir" != "."],
		[
			test -d "$otf_dir" ||
			mkdir -p "$otf_dir" ||
			AC_MSG_ERROR([cannot create $otf_dir])
		])

		cd $otf_dir

		case $srcdir in
			.)
				otf_srcdir="$srcdir"
				;;
			/*)
				otf_srcdir="$srcdir/$otf_dir"
				;;
			*)
				otf_srcdir="../../$srcdir/$otf_dir"
				;;
		esac

		otf_conf_cmd="$otf_srcdir/configure"
		otf_conf_args="--with-openmpi-inside=$inside_openmpi --enable-static=$enable_static --enable-shared=$enable_shared"

		sav_CPPFLAGS=$CPPFLAGS

		AS_IF([test x"$enable_binaries" != "xyes"],
		[
			otf_conf_args="$otf_conf_args --disable-binaries"
		])
		AS_IF([test x"$cross_compiling" = "xyes"],
		[
			AS_IF([test ! -z $build], [otf_conf_args="$otf_conf_args --build=$build"])
			AS_IF([test ! -z $host], [otf_conf_args="$otf_conf_args --host=$host"])
		])
		AS_IF([test x"$have_mpi" = "xyes"],
		[
			export MPICC
			export MPICXX
			AS_IF([test x"$force_mpi" = "xyes"],
			[otf_conf_args="$otf_conf_args --with-mpi"])
			AS_IF([test $MPICC = $CC],
			[
				AS_IF([test x"$MPIDIR" != x],
				[otf_conf_args="$otf_conf_args --with-mpi-dir=$MPIDIR"])
dnl				if we are inside Open MPI package MPIINCDIR contains multiple paths
dnl				so it cannot be used for --with-mpi-inc-dir; use CPPFLAGS instead
				AS_IF([test x"$MPIINCDIR" != x],
				[CPPFLAGS="$CPPFLAGS $MPIINCDIR"])
dnl				[otf_conf_args="$otf_conf_args --with-mpi-inc-dir=`echo $MPIINCDIR | sed s/-I//`"])
				AS_IF([test x"$MPILIBDIR" != x],
				[otf_conf_args="$otf_conf_args --with-mpi-lib-dir=`echo $MPILIBDIR | sed s/-L//`"])
				AS_IF([test x"$MPILIB" != x],
				[otf_conf_args="$otf_conf_args --with-mpi-lib=\"$MPILIB\""])
			])
		],
		[
			otf_conf_args="$otf_conf_args --without-mpi"
		])
		AS_IF([test x"$have_zlib" = "xyes"],
		[
			AS_IF([test x"$force_zlib" = "xyes"],
			[otf_conf_args="$otf_conf_args --with-zlib"])
			AS_IF([test x"$ZLIBDIR" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-dir=$ZLIBDIR"])
			AS_IF([test x"$ZLIBINCDIR" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-inc-dir=`echo $ZLIBINCDIR | sed s/-I//`"])
			AS_IF([test x"$ZLIBLIBDIR" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-lib-dir=`echo $ZLIBLIBDIR | sed s/-L//`"])
			AS_IF([test x"$ZLIBLIB" != x],
			[otf_conf_args="$otf_conf_args --with-zlib-lib=\"$ZLIBLIB\""])
		],
		[
			otf_conf_args="$otf_conf_args --without-zlib"
		])

		otf_conf_args="$otf_conf_args --prefix=\"$prefix\" --exec-prefix=\"$exec_prefix\" --bindir=\"$bindir\" --libdir=\"$libdir\" --includedir=\"$includedir\" --docdir=\"$docdir/otf\" $OTFFLAGS --cache-file=\"/dev/null\" --srcdir=\"$otf_srcdir\""
		
		AC_MSG_NOTICE([running $SHELL $otf_conf_cmd $otf_conf_args])
		eval "$SHELL '$otf_conf_cmd' $otf_conf_args"
		AS_IF([test $? != "0"], [AC_MSG_ERROR([$otf_conf_cmd failed for $otf_dir])])

		CPPFLAGS=$sav_CPPFLAGS

		cd $otf_parent_dir

		OTFINCDIR=
		OTFLIBDIR=
		AS_IF([test x"$OTFLIB" = x], [OTFLIB="-lotf $ZLIBLIBDIR $ZLIBLIB"])
	])

	AC_SUBST(OTFDIR)
	AC_SUBST(OTFINCDIR)
	AC_SUBST(OTFLIBDIR)
	AC_SUBST(OTFLIB)
])


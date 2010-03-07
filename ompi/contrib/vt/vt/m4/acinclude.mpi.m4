AC_DEFUN([ACVT_MPI],
[
	mpi_error="no"
	openmpi="no"
	have_mpio=

	MPIDIR=
	MPIINCDIR=
	MPILIBDIR=
	MPILIB=
	PMPILIB=
	FMPILIB=

	AC_ARG_VAR(MPICC, [MPI C compiler command])
	AC_ARG_VAR(MPICFLAGS, [MPI C compiler flags (append to CFLAGS)])

	AC_ARG_WITH(mpi-dir,
		AC_HELP_STRING([--with-mpi-dir=MPIDIR], [give the path for MPI, default: /usr]),
	[MPIDIR="$withval/"])

	AC_ARG_WITH(mpi-inc-dir,
		AC_HELP_STRING([--with-mpi-inc-dir=MPIINCDIR],
		[give the path for MPI-include files, default: MPIDIR/include]),
	[MPIINCDIR="-I$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPIINCDIR="-I$MPIDIR"include/])])

	AC_ARG_WITH(mpi-lib-dir,
		AC_HELP_STRING([--with-mpi-lib-dir=MPILIBDIR],
		[give the path for MPI-libraries, default: MPIDIR/lib]),
	[MPILIBDIR="-L$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPILIBDIR="-L$MPIDIR"lib/])])

	AC_ARG_WITH(mpi-lib,
		AC_HELP_STRING([--with-mpi-lib=MPILIB], [use given mpi lib]),
	[MPILIB="$withval"])

	AC_ARG_WITH(pmpi-lib,
		AC_HELP_STRING([--with-pmpi-lib=PMPILIB], [use given pmpi lib, default: MPILIB]),
	[PMPILIB="$withval"])

	AC_ARG_WITH(openmpi,
		AC_HELP_STRING([--with-openmpi], [configure for building in Open MPI]),
	[AS_IF([test x"$withval" = "xyes"], [openmpi="yes"])])

	AC_ARG_ENABLE(mpi-io,
		AC_HELP_STRING(--enable-mpi-io,
		[MPI supports file access, default: yes if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [have_mpio="yes"], [have_mpio="no"])])

	AS_IF([test "$openmpi" = "yes"],
	[
		CPPFLAGS="-DINSIDE_OPENMPI $CPPFLAGS"
		MPICC="$CC"
		MPIINCDIR="-I$top_vt_builddir/../../../include"
		MPILIB="-lmpi"
		AS_IF([test x"$have_mpio" = x], [have_mpio="yes"])
		AC_MSG_NOTICE([skipped further tests for MPI])
	],
	[
		AC_CHECK_PROGS(MPICC, mpicc hcc mpcc mpcc_r mpxlc mpixlc cmpicc mpiicc)

		AS_IF([test x"$MPICC" != x],
		[
			mpicc=`echo $MPICC | cut -d ' ' -f 1`
			which_mpicc=`which $mpicc 2>/dev/null`
			AS_IF([test x"$which_mpicc" = x], [AC_MSG_ERROR([$mpicc not found!])])

			mpi_bin_dir=`dirname $which_mpicc`
			AS_IF([test "$mpi_bin_dir" != "/usr/bin"],
			[
				AS_IF([test x"$MPIDIR" = x],
				[MPIDIR=`echo $mpi_bin_dir | sed -e 's/bin//'`])
				AS_IF([test x"$MPIINCDIR" = x],
				[MPIINCDIR=-I`echo $mpi_bin_dir | sed -e 's/bin/include/'`])
				AS_IF([test x"$MPILIBDIR" = x],
				[MPILIBDIR=-L`echo $mpi_bin_dir | sed -e 's/bin/lib/'`])
			])
		],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $MPIINCDIR"
			AC_CHECK_HEADER([mpi.h], [MPICC="$CC"],
			[
				AC_MSG_NOTICE([error: no mpi.h found; check path for MPI package first...])
				mpi_error="yes"
			])
			CPPFLAGS=$sav_CPPFLAGS
		])

dnl		check for Open MPI

		AS_IF([test "$mpi_error" = "no"],
		[
			sav_CC=$CC
			sav_CFLAGS=$CFLAGS
			CC=$MPICC
			CFLAGS="$CFLAGS $MPIINCDIR"
			AC_CHECK_DECL([OPEN_MPI], [],
			[
				AC_MSG_NOTICE([error: unsupported MPI implementation; This version of VampirTrace does only support Open MPI.])
				mpi_error="yes"
			], [#include "mpi.h"])
			CC=$sav_CC
			CFLAGS=$sav_CFLAGS
		])

dnl		check for MPILIB

		AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpi"
			AC_MSG_CHECKING([whether linking with -lmpi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmpi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$MPILIB" = x -a "$mpi_error" = "no"],
		[
			AC_MSG_NOTICE([error: no libmpi found; check path for MPI package first...])
			mpi_error="yes"
		])

dnl		check for MPI I/O

		AS_IF([test x"$have_mpio" = x -a "$mpi_error" = "no"],
		[
			sav_CC=$CC
			sav_CFLAGS=$CFLAGS
			sav_LIBS=$LIBS
			CC=$MPICC
			CFLAGS="$CFLAGS $MPIINCDIR"
			LIBS="$LIBS $MPILIBDIR $MPILIB"
			AC_CHECK_FUNC([MPI_File_open], [have_mpio="yes"], [have_mpio="no"])
			CC=$sav_CC
			CFLAGS=$sav_CFLAGS
			LIBS=$sav_LIBS
		])
	])

	AS_IF([test "$mpi_error" = "no"],
	[
		AS_IF([test x"$PMPILIB" = x"$MPILIB"], [PMPILIB=])
		FMPILIB="-lvt.fmpi"

		AS_IF([test "$have_mpio" = "yes"],
		[AC_DEFINE([HAVE_MPIO], [1], [Define to 1 if MPI supports file access.])])
	],
	[
		MPICC="$CC"
	])

	AC_SUBST(MPIDIR)
	AC_SUBST(MPIINCDIR)
	AC_SUBST(MPILIBDIR)
	AC_SUBST(MPILIB)
	AC_SUBST(PMPILIB)
	AC_SUBST(FMPILIB)
])


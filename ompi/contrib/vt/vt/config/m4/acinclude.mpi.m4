AC_DEFUN([ACVT_MPI],
[
	mpi_error="no"
	check_mpi="yes"
	force_mpi="no"
	have_mpi="no"
	have_fmpi="no"

	check_mpi2_thread="yes"; force_mpi2_thread="no"; have_mpi2_thread="no"
	check_mpi2_1sided="yes"; force_mpi2_1sided="no"; have_mpi2_1sided="no"
	check_mpi2_extcoll="yes"; force_mpi2_extcoll="no"; have_mpi2_extcoll="no"
	check_mpi2_io="yes"; force_mpi2_io="no"; have_mpi2_io="no"

	check_fmpiwraplib="yes"
	force_fmpiwraplib="no"
	build_fmpiwraplib="no"
	check_fc_conv="yes"
	have_mpi2_const="no"
	have_mpi_status_size="no"

	MPIDIR=
	MPIINCDIR=
	FMPIINCDIR=
	MPILIBDIR=
	MPILIB=
	PMPILIB=
	FMPILIB=

	VT_MPIGEN_HAVE_MPI2_THREAD=0
	VT_MPIGEN_HAVE_MPI2_1SIDED=0
	VT_MPIGEN_HAVE_MPI2_EXTCOLL=0
	VT_MPIGEN_HAVE_MPI2_IO=0

	AC_ARG_VAR(MPICC, [MPI C compiler command])
	AC_ARG_VAR(MPICXX, [MPI C++ compiler command])
	AC_ARG_VAR(MPIF77, [MPI Fortran 77 compiler command])
	AC_ARG_VAR(MPICFLAGS, [MPI C compiler flags (append to CFLAGS)])
	AC_ARG_VAR(MPICXXFLAGS, [MPI C++ compiler flags (append to CXXFLAGS)])
	AC_ARG_VAR(MPIFFLAGS, [MPI Fortran 77 compiler flags (append to FFLAGS)])

	AS_IF([test x"$inside_openmpi" = "xyes"],
	[
		AC_MSG_NOTICE([we are configuring inside Open MPI; presetting cache to skip MPI related tests])

		ac_cv_prog_MPICC="$CC"
		ac_cv_prog_MPICXX="$CXX"
		ac_cv_prog_MPIF77="$F77"

		MPIINCDIR="-I$top_vt_srcdir/../../../include -I$top_vt_builddir/../../../include"
		FMPIINCDIR="$MPIINCDIR"
		# MPILIBDIR is used in the compiler wrapper configuration files; set LDFLAGS instead
		LDFLAGS="$LDFLAGS -L$top_vt_builddir/../../../.libs"

		enable_mpi="yes"
		with_openmpi="yes"

		check_mpi2_io="no"; have_mpi2_io="yes"
		AC_ARG_ENABLE(mpi-io,
			AC_HELP_STRING([--enable-mpi-io], [equal to --enable-mpi2-io]),
		[AS_IF([test x"$enableval" = "xno"], [have_mpi2_io="no"])])
	])

	AC_ARG_ENABLE(mpi,
		AC_HELP_STRING([--enable-mpi],
		[enable MPI support, default: enable if MPI-installation found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_mpi="yes"], [check_mpi="no"])])

	AC_ARG_WITH(mpi-dir,
		AC_HELP_STRING([--with-mpi-dir=MPIDIR], [give the path for MPI, default: /usr]),
	[MPIDIR="$withval/"])

	AC_ARG_WITH(mpi-inc-dir,
		AC_HELP_STRING([--with-mpi-inc-dir=MPIINCDIR],
		[give the path for MPI-include files, default: MPIDIR/include]),
	[MPIINCDIR="-I$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPIINCDIR="-I$MPIDIR"include/])])

	AC_ARG_WITH(fmpi-inc-dir,
		AC_HELP_STRING([--with-fmpi-inc-dir=FMPIINCDIR],
		[give the path for Fortran MPI-include files, default: MPIINCDIR]),
	[FMPIINCDIR="-I$withval/"],
	[
		FMPIINCDIR=$MPIINCDIR
		AS_IF([test x"$FMPIINCDIR" = x], [FMPIINCDIR="-I/usr/include"])
	])

	AC_ARG_WITH(mpi-lib-dir,
		AC_HELP_STRING([--with-mpi-lib-dir=MPILIBDIR],
		[give the path for MPI-libraries, default: MPIDIR/lib]),
	[MPILIBDIR="-L$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPILIBDIR="-L$MPIDIR"lib/])])

	AC_ARG_WITH(hpmpi,
		AC_HELP_STRING([--with-hpmpi], [set MPI-libs for HP MPI]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmtmpi"
			PMPILIB="-lmtpmpi"
			FMPILIB="-lvt-fmpi"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(intelmpi,
		AC_HELP_STRING([--with-intelmpi], [set MPI-libs for Intel MPI]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-lvt-fmpi"
			check_mpi2_thread="no"; have_mpi2_thread="no"
			check_mpi2_1sided="no"; have_mpi2_1sided="no"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			check_mpi2_io="no"; have_mpi2_io="no"
			ac_cv_have_decl_MPI_IN_PLACE="no"
		])
	])

	AC_ARG_WITH(intelmpi2,
		AC_HELP_STRING([--with-intelmpi2], [set MPI-libs for Intel MPI2]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-lvt-fmpi"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(lam,
		AC_HELP_STRING([--with-lam], [set MPI-libs for LAM/MPI]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi -llam"
			PMPILIB="$MPILIB"
			FMPILIB="-llamf77mpi"
			check_mpi2_io="no"; have_mpi2_io="no"
		])
	])

	AC_ARG_WITH(mpibgl,
		AC_HELP_STRING([--with-mpibgl], [set MPI-libs for IBM BG/L]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpich.rts"
			PMPILIB="-lmpich.rts"
			FMPILIB="-lfmpich.rts"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
		])
	])

	AC_ARG_WITH(mpibgp,
		AC_HELP_STRING([--with-mpibgp], [set MPI-libs for IBM BG/P]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpich.cnk"
			PMPILIB="-lmpich.cnk"
			FMPILIB="-lfmpich.cnk"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(mpich,
		AC_HELP_STRING([--with-mpich], [set MPI-libs for MPICH]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpich"
			PMPILIB="-lpmpich"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="no"
			check_mpi2_1sided="no"; have_mpi2_1sided="no"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			check_mpi2_io="no"; have_mpi2_io="no"
		])
	])

	AC_ARG_WITH(mpich2,
		AC_HELP_STRING([--with-mpich2], [set MPI-libs for MPICH2]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpich"
			PMPILIB="$MPILIB"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(mvapich,
		AC_HELP_STRING([--with-mvapich], [set MPI-libs for MVAPICH]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpich"
			PMPILIB="-lpmpich"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="no"
			check_mpi2_1sided="no"; have_mpi2_1sided="no"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			check_mpi2_io="no"; have_mpi2_io="no"
		])
	])

	AC_ARG_WITH(mvapich2,
		AC_HELP_STRING([--with-mvapich2], [set MPI-libs for MVAPICH2]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpich"
			PMPILIB="$MPILIB"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(mpisx,
		AC_HELP_STRING([--with-mpisx], [set MPI-libs for NEC MPI/SX]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi"
			PMPILIB="-lpmpi"
			FMPILIB="-lfmpi"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			check_mpi2_io="no"; have_mpi2_io="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(mpisx-ew,
		AC_HELP_STRING([--with-mpisx-ew], [set MPI-libs for NEC MPI/SX with Fortran -ew]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpiw"
			PMPILIB="-lpmpiw"
			FMPILIB="-lfmpiw"
			MPICFLAGS="$MPICFLAGS -D_W8"
			MPICXXFLAGS="$MPICXXFLAGS -D_W8"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			check_mpi2_io="no"; have_mpi2_io="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(openmpi,
		AC_HELP_STRING([--with-openmpi], [set MPI-libs for Open MPI]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-lmpi_f77"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_func_MPI_Add_error_class="yes"
			ac_cv_func_MPI_Add_error_code="yes"
			ac_cv_func_MPI_Add_error_string="yes"
			ac_cv_func_MPI_Get_address="yes"
			ac_cv_func_MPI_Finalized="yes"
			ac_cv_func_MPI_Type_create_f90_complex="yes"
			ac_cv_func_MPI_Type_create_f90_integer="yes"
			ac_cv_func_MPI_Type_create_f90_real="yes"
			ac_cv_func_MPI_Type_create_struct="yes"
			ac_cv_func_MPI_Type_match_size="yes"
			ac_cv_func_PMPI_Win_test="yes"
			ac_cv_func_PMPI_Win_lock="yes"
			ac_cv_func_PMPI_Win_unlock="yes"
			AS_IF([test x"$inside_openmpi" = "xyes" -a x"$have_mpi2_io" = "xyes"],
			[
				ac_cv_func_PMPI_File_read_ordered="yes"
				ac_cv_func_PMPI_File_read_ordered_begin="yes"
				ac_cv_func_PMPI_File_write_ordered="yes"
				ac_cv_func_PMPI_File_write_ordered_begin="yes"
				ac_cv_func_MPI_Register_datarep="yes"
			])
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(sgimpt,
		AC_HELP_STRING([--with-sgimpt], [set MPI-libs for SGI MPT]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-lvt-fmpi"
			check_fc_conv="no"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(sunmpi,
		AC_HELP_STRING([--with-sunmpi], [set MPI-libs for SUN MPI]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-lvt-fmpi"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(sunmpi-mt,
		AC_HELP_STRING([--with-sunmpi-mt], [set MPI-libs for SUN MPI-MT]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			MPILIB="-lmpi_mt"
			PMPILIB="$MPILIB"
			FMPILIB="-lvt-fmpi"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(mpibull2,
		AC_HELP_STRING([--with-mpibull2], [set MPI-libs for Bull MPICH2]),
	[
		AS_IF([test x"$withval" = "xyes" -a x"$inside_openmpi" = "xno"],
		[
			AS_IF([test x"$MPILIBDIR" = x],
			[pmilibdir="-L/usr/lib/pmi"], [pmilibdir="$MPILIBDIR/pmi"])

			MPILIBDIR="$MPILIBDIR $pmilibdir"
			MPILIB="-lmpi -lpmi"
			PMPILIB="$MPILIB"
			FMPILIB="-lmpibinding_f77"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
		])
	])

	AC_ARG_WITH(mpi-lib,
		AC_HELP_STRING([--with-mpi-lib], [use given mpi lib]),
	[MPILIB="$withval"])

	AC_ARG_WITH(pmpi-lib,
		AC_HELP_STRING([--with-pmpi-lib], [use given pmpi lib]),
	[PMPILIB="$withval"])

	AC_ARG_WITH(fmpi-lib,
		AC_HELP_STRING([--with-fmpi-lib], [use given fmpi lib]),
	[FMPILIB="$withval"])

	AC_ARG_ENABLE(fmpi-lib,
		AC_HELP_STRING([--enable-fmpi-lib],
		[build MPI Fortran support library, default: enable if no MPI Fortran library found by configure]), 
	[AS_IF([test x"$enableval" = "xyes"],
	[force_fmpiwraplib="yes"; FMPILIB="-lvt-fmpi"], [check_fmpiwraplib="no"])])

	AC_ARG_ENABLE(fmpi-handle-convert,
		AC_HELP_STRING([--enable-fmpi-handle-convert],
		[do convert MPI handles, default: enable if MPI conversion functions found by configure]),
	[AS_IF([test x"$enableval" != "xyes"], [check_fc_conv="no"])])

	AC_ARG_ENABLE(mpi2-thread,
		AC_HELP_STRING([--enable-mpi2-thread],
		[enable MPI-2 Thread support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_thread="yes"; force_mpi2_thread="yes"; have_mpi2_thread="no"],
	[check_mpi2_thread="no"]; force_mpi2_thread="no"; have_mpi2_thread="no")])

	AC_ARG_ENABLE(mpi2-1sided,
		AC_HELP_STRING([--enable-mpi2-1sided],
		[enable MPI-2 One-Sided Communication support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_1sided="yes"; force_mpi2_1sided="yes"; have_mpi2_1sided="no"],
	[check_mpi2_1sided="no"]; force_mpi2_1sided="no"; have_mpi2_1sided="no")])

	AC_ARG_ENABLE(mpi2-extcoll,
		AC_HELP_STRING([--enable-mpi2-extcoll],
		[enable MPI-2 Extended Collective Operation support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_extcoll="yes"; force_mpi2_extcoll="yes"; have_mpi2_extcoll="no"],
	[check_mpi2_extcoll="no"; force_mpi2_extcoll="no"; have_mpi2_extcoll="no"])])

	AC_ARG_ENABLE(mpi2-io,
		AC_HELP_STRING([--enable-mpi2-io],
		[enable MPI-2 I/O support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_io="yes"; force_mpi2_io="yes"; have_mpi2_io="no"],
	[check_mpi2_io="no"; force_mpi2_io="no"; have_mpi2_io="no"])])

	AS_IF([test x"$check_mpi" = "xyes"],
	[
		AC_CHECK_PROGS(MPICC, mpicc hcc mpcc_r mpcc mpxlc_r mpxlc mpixlc_r mpixlc cmpicc mpiicc)
		AS_IF([test x"$MPICC" != x],
		[
			AS_IF([test x"$inside_openmpi" = "xno"],
			[
				mpicc=`echo $MPICC | cut -d ' ' -f 1`
				which_mpicc=`which $mpicc 2>/dev/null`
				AS_IF([test x"$which_mpicc" = x], [AC_MSG_ERROR([$mpicc not found])])

				mpi_bin_dir=`dirname $which_mpicc`
				AS_IF([test "$mpi_bin_dir" != "/usr/bin" -a "$mpi_bin_dir" != "/SX/usr/bin"],
				[
					AS_IF([test x"$MPIDIR" = x],
					[MPIDIR=`echo $mpi_bin_dir | sed -e 's/bin//'`])
					AS_IF([test x"$MPIINCDIR" = x],
					[MPIINCDIR=-I`echo $mpi_bin_dir | sed -e 's/bin/include/'`])
					AS_IF([test x"$FMPIINCDIR" = x],
					[FMPIINCDIR=$MPIINCDIR])
					AS_IF([test x"$MPILIBDIR" = x],
					[MPILIBDIR=-L`echo $mpi_bin_dir | sed -e 's/bin/lib/'`])
				])
			])
		],
		[
			MPICC="$CC"
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $MPIINCDIR"
			AC_CHECK_HEADER([mpi.h], [],
			[
				AC_MSG_NOTICE([error: no mpi.h found; check path for MPI package first...])
				mpi_error="yes"
			])
			CPPFLAGS=$sav_CPPFLAGS
		])

		AS_IF([test x"$mpi_error" = "xno"],
		[
			AC_CHECK_PROGS(MPICXX, mpicxx mpic++ mpiCC hcp mpxlC_r mpxlC mpCC_r mpCC cmpic++)
			AS_IF([test x"$MPICXX" = x], [MPICXX="$CXX"])

			MPICXXFLAGS="$MPICXXFLAGS -DMPICH_SKIP_MPICXX -DOMPI_SKIP_MPICXX -DMPI_NO_CPPBIND"
		])

dnl		check for MPILIB

		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpi_r"
			AC_MSG_CHECKING([whether linking with -lmpi_r works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmpi_r],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpi"
			AC_MSG_CHECKING([whether linking with -lmpi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmpi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS

			AS_IF([test x"$MPILIB" != x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -llam"
				AC_MSG_CHECKING([whether linking with -llam works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIB="-lmpi -llam"],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])
		])

		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			AS_IF([test x"$MPILIBDIR" = x],
			[pmilibdir="-L/usr/lib/pmi"], [pmilibdir="$MPILIBDIR/pmi"])

			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR $pmilibdir -lmpi -lpmi"
			AC_MSG_CHECKING([whether linking with -lmpi $pmilibdir -lpmi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIBDIR="$MPILIBDIR $pmilibdir"; MPILIB="-lmpi -lpmi"],
			[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])
			
		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpich"
			AC_MSG_CHECKING([whether linking with -lmpich works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmpich],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
			AS_IF([test x"$MPILIB" != x],
			[MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"])
		])

		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpichg2"
			AC_MSG_CHECKING([whether linking with -lmpichg2 works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmpichg2],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
			AS_IF([test x"$MPILIB" != x],
			[MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"])
		])

		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lhpmpi"
			AC_MSG_CHECKING([whether linking with -lhpmpi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lhpmpi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
			AS_IF([test x"$MPILIB" != x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmtmpi"
				AC_MSG_CHECKING([whether linking with -lmtmpi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIB=-lmtmpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])
		])

		AS_IF([test x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libmpi_r, libmpi, liblam, libmpich, or libhpmpi found; check path for MPI package first...])
			mpi_error="yes"
		])

dnl		check for PMPILIB

		AS_IF([test x"$PMPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lpmpich"
			AC_MSG_CHECKING([whether linking with -lpmpich works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); PMPILIB=-lpmpich],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$PMPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lpmpichg2"
			AC_MSG_CHECKING([whether linking with -lpmpichg2 works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); PMPILIB=-lpmpichg2],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$PMPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lpmpi"
			AC_MSG_CHECKING([whether linking with -lpmpi works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); PMPILIB=-lpmpi],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
			AS_IF([test x"$PMPILIB" != x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmtpmpi"
				AC_MSG_CHECKING([whether linking with -lmtpmpi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); PMPILIB=-lmtpmpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])
		])

		AS_IF([test x"$PMPILIB" = x -a x"$mpi_error" = "xno"],
		[
			PMPILIB="$MPILIB"
			AC_MSG_WARN([no libpmpich, or libpmpi found; assuming $MPILIB])
		])

dnl		check for FMPILIB

		AS_IF([test x"$F77" != x],
		[
			AS_IF([test x"$FMPILIB" = x -a x"$mpi_error" = "xno"],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpi_f77 $MPILIB"
				AC_MSG_CHECKING([whether linking with -lmpi_f77 works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-lmpi_f77],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x -a x"$mpi_error" = "xno"],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpibinding_f77"
				AC_MSG_CHECKING([whether linking with -lmpibinding_f77 works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-lmpibinding_f77],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x -a x"$mpi_error" = "xno"],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lfmpich"
				AC_MSG_CHECKING([whether linking with -lfmpich works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-lfmpich],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x -a x"$mpi_error" = "xno"],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -llamf77mpi"
				AC_MSG_CHECKING([whether linking with -llamf77mpi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-llamf77mpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x -a x"$mpi_error" = "xno"],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lfmpi"
				AC_MSG_CHECKING([whether linking with -lfmpi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-lfmpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$mpi_error" = "xno"],
			[
				AS_IF([test x"$FMPILIB" = x],
				[
					AS_IF([test x"$check_fmpiwraplib" = "xyes"],
					[
						AC_MSG_WARN([no libmpi_f77, libmpibinding_f77, libfmpich, liblamf77mpi, or libfmpi found; build libvt-fmpi])
						FMPILIB="-lvt-fmpi"
					])
				],
				[
					AS_IF([test x"$FMPILIB" = "x-lvt-fmpi"],
					[
						AS_IF([test x"$check_fmpiwraplib" = "xno"],
						[FMPILIB=])
					],
					[
						AS_IF([test x"$force_fmpiwraplib" = "xyes"],
						[FMPILIB="-lvt-fmpi"], [check_fmpiwraplib="no"])
					])
				])
			])
		])

dnl		check for MPI-2

		AS_IF([test x"$mpi_error" = "xno"],
		[
			sav_CC=$CC
			sav_CPPFLAGS=$CPPFLAGS
			sav_LIBS=$LIBS
			CC=$MPICC
			CPPFLAGS="$CPPFLAGS $MPICFLAGS $MPIINCDIR"
			LIBS="$LIBS $MPILIBDIR $MPILIB $PMPILIB"

dnl			check for MPI-2 constants

			ACVT_CONF_SUBTITLE([MPI-2 constants])

			AC_CHECK_DECLS([MPI_IN_PLACE], [], [], [#include "mpi.h"])

dnl			check for MPI-2 functions

			ACVT_CONF_SUBTITLE([MPI-2 functions])

			AC_CHECK_FUNCS([MPI_Add_error_class \
                                        MPI_Add_error_code \
                                        MPI_Add_error_string \
                                        MPI_Get_address \
                                        MPI_Finalized \
                                        MPI_Type_create_f90_complex \
                                        MPI_Type_create_f90_integer \
                                        MPI_Type_create_f90_real \
                                        MPI_Type_create_struct \
                                        MPI_Type_match_size])
			
dnl			check for MPI-2 Thread support

			ACVT_CONF_SUBSUBTITLE([MPI-2 Thread support])
			AS_IF([test x"$check_mpi2_thread" = "xyes"],
			[
				AC_CHECK_FUNC([MPI_Init_thread], [have_mpi2_thread="yes"])
				AS_IF([test x"$force_mpi2_thread" = "xyes" -a x"$have_mpi2_thread" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_thread" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])

dnl			check for MPI-2 One-Sided Communications

			ACVT_CONF_SUBSUBTITLE([MPI-2 One-Sided Communications])
			AS_IF([test x"$check_mpi2_1sided" = "xyes"],
			[
				AC_CHECK_FUNC([MPI_Get],
				 [AC_CHECK_FUNC([MPI_Put], [have_mpi2_1sided="yes"])])
				AS_IF([test x"$force_mpi2_1sided" = "xyes" -a x"$have_mpi2_1sided" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_1sided" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])
                        AS_IF([test x"$have_mpi2_1sided" = "xyes"],
			[
                        	AC_CHECK_FUNCS([PMPI_Win_test \
                                                PMPI_Win_lock \
                                                PMPI_Win_unlock])
			])

dnl			check for MPI-2 Extended Collective Operations

			ACVT_CONF_SUBSUBTITLE([MPI-2 Extended Collective Operations])
			AS_IF([test x"$check_mpi2_extcoll" = "xyes"],
			[
				AC_CHECK_FUNC([MPI_Alltoallw],
				 [AC_CHECK_FUNC([MPI_Exscan], [have_mpi2_extcoll="yes"])])
				AS_IF([test x"$force_mpi2_extcoll" = "xyes" -a x"$have_mpi2_extcoll" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_extcoll" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])

dnl			check for MPI-2 I/O

			ACVT_CONF_SUBSUBTITLE([MPI-2 I/O])
			AS_IF([test x"$check_mpi2_io" = "xyes"],
			[
				AC_CHECK_DECL([LAM_MPI],
				[
					AC_MSG_NOTICE([error: MPI-2 I/O isn't supported for LAM/MPI])
				],
				[
					AC_CHECK_FUNC([MPI_File_open],
					 [AC_CHECK_FUNC([MPI_File_close], [have_mpi2_io="yes"])])
				], [#include "mpi.h"])
				AS_IF([test x"$force_mpi2_io" = "xyes" -a x"$have_mpi2_io" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_io" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])
			AS_IF([test x"$have_mpi2_io" = "xyes"],
			[
				AC_CHECK_FUNCS([MPI_Register_datarep \
                                                PMPI_File_read_ordered \
                                                PMPI_File_read_ordered_begin \
                                                PMPI_File_write_ordered \
                                                PMPI_File_write_ordered_begin])
			])
	
			CC=$sav_CC
			CPPFLAGS=$sav_CPPFLAGS
			LIBS=$sav_LIBS
		])

dnl		check for Fortran interoperability

		AS_IF([test x"$check_fmpiwraplib" = "xyes" -a x"$mpi_error" = "xno"],
		[
			ACVT_CONF_SUBTITLE([Fortran interoperability])
			ACVT_FMPIWRAPLIB
			AS_IF([test x"$fmpiwraplib_error" = "xno"],
			[build_fmpiwraplib="yes"], [MPIF77="$F77"; FMPILIB=])
		])

		AS_IF([test x"$mpi_error" = "xno"], [have_mpi="yes"],
		[MPICC="$CC"; MPICXX="$CXX"; MPIF77="$F77"])
		AS_IF([test x"$FMPILIB" != x], [have_fmpi="yes"])
		AS_IF([test x"$have_mpi2_thread" = "xyes"], [VT_MPIGEN_HAVE_MPI2_THREAD=1])
		AS_IF([test x"$have_mpi2_1sided" = "xyes"], [VT_MPIGEN_HAVE_MPI2_1SIDED=1])
		AS_IF([test x"$have_mpi2_extcoll" = "xyes"], [VT_MPIGEN_HAVE_MPI2_EXTCOLL=1])
		AS_IF([test x"$have_mpi2_io" = "xyes"], [VT_MPIGEN_HAVE_MPI2_IO=1])
	])

	AS_IF([test x"$have_mpi" = "xyes"],
	[AC_DEFINE([HAVE_MPI], [1], [Define to 1 if VT is configured with MPI support.])])

	AS_IF([test x"$have_fmpi" = "xyes"],
	[AC_DEFINE([HAVE_FMPI], [1], [Define to 1 if VT is configured with MPI Fortran support.])])

	AS_IF([test x"$have_mpi2_thread" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_THREAD], [1], [Define to 1 if you have functions for MPI-2 Thread support.])])

	AS_IF([test x"$have_mpi2_1sided" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_1SIDED], [1], [Define to 1 if you have functions for MPI-2 One-Sided Communications.])])

	AS_IF([test x"$have_mpi2_extcoll" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_EXTCOLL], [1], [Define to 1 if you have functions for MPI-2 Extended Collective Operations.])])

	AS_IF([test x"$have_mpi2_io" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_IO], [1], [Define to 1 if you have functions for MPI-2 I/O.])])

	AC_SUBST(MPIDIR)
	AC_SUBST(MPIINCDIR)
	AC_SUBST(FMPIINCDIR)
	AC_SUBST(MPILIBDIR)
	AC_SUBST(MPILIB)
	AC_SUBST(PMPILIB)
	AC_SUBST(FMPILIB)

	AC_SUBST(VT_MPIGEN_HAVE_MPI2_THREAD)
	AC_SUBST(VT_MPIGEN_HAVE_MPI2_1SIDED)
	AC_SUBST(VT_MPIGEN_HAVE_MPI2_EXTCOLL)
	AC_SUBST(VT_MPIGEN_HAVE_MPI2_IO)
])

AC_DEFUN([ACVT_FMPIWRAPLIB],
[
	fmpiwraplib_error="no"

	VT_MPIGEN_HAVE_FC_CONV_COMM=0
	VT_MPIGEN_HAVE_FC_CONV_ERRH=0
	VT_MPIGEN_HAVE_FC_CONV_FILE=0
	VT_MPIGEN_HAVE_FC_CONV_GROUP=0
	VT_MPIGEN_HAVE_FC_CONV_INFO=0
	VT_MPIGEN_HAVE_FC_CONV_OP=0
	VT_MPIGEN_HAVE_FC_CONV_REQUEST=0
	VT_MPIGEN_HAVE_FC_CONV_STATUS=0
	VT_MPIGEN_HAVE_FC_CONV_TYPE=0
	VT_MPIGEN_HAVE_FC_CONV_WIN=0
	VT_MPIGEN_HAVE_FC_CONV_MPI2CONST=0

	AS_IF([test x"$F77" != x],
	[
		AC_CHECK_PROGS(MPIF77, mpif77 hf77 mpxlf_r mpxlf mpf77 cmpifc mpif90 mpxlf95_r mpxlf90_r mpxlf95 mpxlf90 mpf90 cmpif90c)
		AS_IF([test x"$MPIF77" != x],
		[
			AS_IF([test x"$inside_openmpi" = "xno"],
			[
				mpif77=`echo $MPIF77 | cut -d ' ' -f 1`
				which_mpif77=`which $mpif77 2>/dev/null`
				AS_IF([test x"$which_mpif77" = x], [AC_MSG_ERROR([$mpif77 not found])])

				mpi_bin_dir=`dirname $which_mpif77`
				AS_IF([test "$mpi_bin_dir" != "/usr/bin" -a "$mpi_bin_dir" != "/SX/usr/bin" -a x"$FMPIINCDIR" != x-I"$mpi_inc_dir"],
				[
					mpi_inc_dir=-I`echo $mpi_bin_dir | sed -e 's/bin/include/'`
					AS_IF([test x"$FMPIINCDIR" != x"$mpi_inc_dir"],
					[FMPIINCDIR="$FMPIINCDIR -I`echo $mpi_bin_dir | sed -e 's/bin/include/'`"])
				])
			])
		],
		[
			MPIF77="$F77"
			AC_MSG_CHECKING([for mpif.h])
			rm -f conftest.f conftest.o
			cat > conftest.f << EOF
      PROGRAM conftest
      INCLUDE 'mpif.h'
      END
EOF
			eval "$F77 $FFLAGS $MPIFFLAGS $FMPIINCDIR -c conftest.f" >/dev/null 2>&1
			AS_IF([test "$?" = "0" -a -s conftest.o], [AC_MSG_RESULT([yes])],
			[
				AC_MSG_RESULT([no])
				AC_MSG_NOTICE([error: no mpif.h found; check path for MPI package first...])
				fmpiwraplib_error="yes"
			])
			rm -f conftest.f conftest.o
		])
	],
	[
		AC_MSG_NOTICE([error: no Fortran 77 compiler command given])
		fmpiwraplib_error="yes"
	])

	AS_IF([test x"$check_fc_conv" = "xyes" -a x"$fmpiwraplib_error" = "xno"],
	[
		sav_CC=$CC
		sav_CPPFLAGS=$CPPFLAGS
		sav_LIBS=$LIBS
		CC=$MPICC
		CPPFLAGS="$CPPFLAGS $MPICFLAGS $MPIINCDIR"
		LIBS="$LIBS $MPILIBDIR $MPILIB"

dnl		check for handle conversion: MPI_Comm
		AC_CHECK_DECL([MPI_Comm_f2c],
		 [AC_CHECK_DECL([MPI_Comm_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_COMM=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Errhandler
		AC_CHECK_DECL([MPI_Errhandler_f2c],
		 [AC_CHECK_DECL([MPI_Errhandler_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_ERRH=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_File
		AC_CHECK_DECL([MPI_File_f2c],
		 [AC_CHECK_DECL([MPI_File_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_FILE=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Group
		AC_CHECK_DECL([MPI_Group_f2c],
		 [AC_CHECK_DECL([MPI_Group_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_GROUP=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Info
		AC_CHECK_DECL([MPI_Info_f2c],
		 [AC_CHECK_DECL([MPI_Info_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_INFO=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Op
		AC_CHECK_DECL([MPI_Op_f2c],
		 [AC_CHECK_DECL([MPI_Op_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_OP=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Request
		AC_CHECK_DECL([MPI_Request_f2c],
		 [AC_CHECK_DECL([MPI_Request_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_REQUEST=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Status
		AC_CHECK_DECL([MPI_Status_f2c],
		 [AC_CHECK_DECL([MPI_Status_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_STATUS=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Datatype
		AC_CHECK_DECL([MPI_Type_f2c],
		 [AC_CHECK_DECL([MPI_Type_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_TYPE=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for handle conversion: MPI_Win
		AC_CHECK_DECL([MPI_Win_f2c],
		 [AC_CHECK_DECL([MPI_Win_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_WIN=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for MPI-2 constants

		AC_CHECK_DECLS([MPI_IN_PLACE],
		 [VT_MPIGEN_HAVE_FC_CONV_MPI2CONST=1; have_mpi2_const="yes"], [], [#include "mpi.h"])

dnl		check for MPI_STATUS_SIZE
		AC_CHECK_DECLS([MPI_STATUS_SIZE], [have_mpi_status_size="yes"], [], [#include "mpi.h"])

		CC=$sav_CC
		CPPFLAGS=$sav_CPPFLAGS
		LIBS=$sav_LIBS
	])

	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_COMM)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_ERRH)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_FILE)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_GROUP)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_INFO)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_OP)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_REQUEST)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_STATUS)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_TYPE)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_WIN)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_MPI2CONST)
])


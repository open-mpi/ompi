dnl/*D
dnl PAC_PROG_F77_CMDARGS - Determine how to access the command line from
dnl Fortran 77
dnl
dnl Output Effects:
dnl  The following variables are set:
dnl.vb
dnl    F77_GETARG         - Statement to get an argument i into string s
dnl    F77_IARGC          - Routine to return the number of arguments
dnl    FXX_MODULE         - Module command when using Fortran 90 compiler
dnl    F77_GETARGDECL     - Declaration of routine used for F77_GETARG
dnl    F77_GETARG_FFLAGS  - Flags needed when compiling/linking
dnl    F77_GETARG_LDFLAGS - Flags needed when linking
dnl.ve
dnl If 'F77_GETARG' has a value, then that value and the values for these
dnl other symbols will be used instead.  If no approach is found, all of these
dnl variables will have empty values.
dnl If no other approach works and a file 'f77argdef' is in the directory, 
dnl that file will be sourced for the values of the above four variables.
dnl
dnl In most cases, you should add F77_GETARG_FFLAGS to the FFLAGS variable
dnl and F77_GETARG_LDFLAGS to the LDFLAGS variable, to ensure that tests are
dnl performed on the compiler version that will be used.
dnl
dnl 'AC_SUBST' is called for all six variables.
dnl
dnl One complication is that on systems with multiple Fortran compilers, 
dnl some libraries used by one Fortran compiler may have been (mis)placed
dnl in a common location.  We have had trouble with libg2c in particular.
dnl To work around this, we test whether iargc etc. work first.  This
dnl will catch most systems and will speed up the tests.
dnl
dnl Next, the libraries are only added if they are needed to complete a 
dnl link; they aren''t added just because they exist.
dnl
dnl f77argdef
dnl D*/
dnl
dnl Random notes
dnl You can export the command line arguments from C to the g77 compiler
dnl using
dnl    extern char **__libc_argv;
dnl    extern int  __libc_argc;
dnl    f_setarg( __libc_argc, __libc_argv );
dnl
AC_DEFUN([PAC_PROG_F77_CMDARGS],[
found_cached="yes"
AC_MSG_CHECKING([for routines to access the command line from Fortran 77])
AC_CACHE_VAL(pac_cv_prog_f77_cmdarg,
[
    AC_MSG_RESULT([searching...])
    found_cached="no"
    # First, we perform a quick check.  Does iargc and getarg work?
    fxx_module="${FXX_MODULE:-}"
    f77_getargdecl="${F77_GETARGDECL:-external getarg}"
    f77_getarg="${F77_GETARG:-call GETARG(i,s)}"
    f77_iargc="${F77_IARGC:-IARGC()}"
    #    
    # Grumble.  The Absoft Fortran compiler computes i - i as 0 and then
    # 1.0 / 0 at compile time, even though the code may never be executed.
    # What we need is a way to generate an error, so the second usage of i
    # was replaced with f77_iargc.  
    cat > conftest.f <<EOF
        program main
$fxx_module
        integer i, j
        character*20 s
        $f77_getargdecl
        i = 0
        $f77_getarg
        i=$f77_iargc
        if (i .gt. 1) then
            j = i - $f77_iargc
            j = 1.0 / j
        endif
        end
EOF
    found_answer="no"
    if test -z "$ac_fcompilelink" ; then
        ac_fcompilelink="${F77-f77} -o conftest $FFLAGS $flags conftest.f $LDFLAGS $LIBS 1>&AC_FD_CC"
    fi
    AC_MSG_CHECKING([whether ${F77-f77} $flags $libs works with GETARG and IARGC])
    if AC_TRY_EVAL(ac_fcompilelink) && test -x conftest ; then
	# Check that cross != yes so that this works with autoconf 2.52
	# Check that cross_compiling != yes so that this works with 
	# autoconf 2.6x for some (but almost certainly not all)x
	# Question: why do we test that this runs?  It looks like we
	# needed this for some old Fortran compilers that produced
	# executable code that then did not run.
	if test "$ac_cv_prog_f77_cross" != "yes" -a \
	        "$cross_compiling" != "yes" ; then
	    if ./conftest >/dev/null 2>&1 ; then
		found_answer="yes"
	        FXX_MODULE="$fxx_module"
		F77_GETARGDECL="$f77_getargdecl"
		F77_GETARG="$f77_getarg"
		F77_IARGC="$f77_iargc"
		AC_MSG_RESULT(yes)
     	    fi
        fi
    fi    
    if test $found_answer = "no" ; then
	AC_MSG_RESULT(no)
    # Grumph.  Here are a bunch of different approaches
    # We have several axes the check:
    # Library to link with (none, -lU77 (HPUX), -lg2c (LINUX f77))
    # PEPCF90 (Intel ifc)
    # The first line is a dummy
    # (we experimented with using a <space>, but this caused other 
    # problems because we need <space> in the IFS)
    trial_LIBS="0 -lU77 -lPEPCF90"
    if test "$NOG2C" != "1" ; then
        trial_LIBS="$trial_LIBS -lg2c"
    fi
    # Discard libs that are not availble:
    save_IFS="$IFS"
    # Make sure that IFS includes a space, or the tests that run programs
    # may fail
    IFS=" ""
"
    save_trial_LIBS="$trial_LIBS"
    trial_LIBS=""
    cat > conftest.f <<EOF
        program main
        end
EOF
    ac_fcompilelink_test='${F77-f77} -o conftest $FFLAGS conftest.f $LDFLAGS $libs $LIBS 1>&AC_FD_CC'
    for libs in $save_trial_LIBS ; do
	if test "$libs" = "0" ; then
	    lib_ok="yes"
        else
	    AC_MSG_CHECKING([whether Fortran 77 links with $libs])
	    if AC_TRY_EVAL(ac_fcompilelink_test) && test -x conftest ; then
		AC_MSG_RESULT([yes])
	        lib_ok="yes"
	    else
		AC_MSG_RESULT([no])
	        lib_ok="no"
	    fi
	fi
	if test "$lib_ok" = "yes" ; then
	    trial_LIBS="$trial_LIBS
$libs"
        fi
    done

    # Options to use when compiling and linking
    # +U77 is needed by HP Fortran to access getarg etc.
    # The -N109 was used for getarg before we realized that GETARG
    # was necessary with the (non standard conforming) Absoft compiler
    # (Fortran is monocase; Absoft uses mixedcase by default)
    # The -f is used by Absoft and is the compiler switch that folds 
    # symbolic names to lower case.  Without this option, the compiler
    # considers upper- and lower-case letters to be unique.
    # The -YEXT_NAMES=LCS will cause external names to be output as lower
    # case letter for Absoft F90 compilers (default is upper case)
    # The first line is "<space><newline>, the space is important
    # To make the Absoft f77 and f90 work together, we need to prefer the
    # upper case versions of the arguments.  They also require libU77.
    # -YCFRL=1 causes Absoft f90 to work with g77 and similar (f2c-based) 
    # Fortran compilers
    #
    # Problem:  The Intel efc compiler hangs when presented with -N109 .
    # The only real fix for this is to detect this compiler and exclude
    # the test.  We may want to reorganize these tests so that if we
    # can compile code without special options, we never look for them.
    # 
    using_intel_efc="no"
    pac_test_msg=`$F77 -V 2>&1 | grep 'Intel(R) Fortran Itanium'`
    if test "$pac_test_msg" != "" ; then
	using_intel_efc="yes"
    fi
    if test "$using_intel_efc" = "yes" ; then
        trial_FLAGS="000"
    else
        trial_FLAGS="000
-N109
-f
-YEXT_NAMES=UCS
-YEXT_NAMES=LCS
-YCFRL=1
+U77"
    fi
    # Discard options that are not available:
    # (IFS already saved above)
    IFS=" ""
"
    save_trial_FLAGS="$trial_FLAGS"
    trial_FLAGS=""
    for flag in $save_trial_FLAGS ; do
	if test "$flag" = " " -o "$flag" = "000" ; then
	    opt_ok="yes"
        else
            PAC_F77_CHECK_COMPILER_OPTION($flag,opt_ok=yes,opt_ok=no)
        fi
	if test "$opt_ok" = "yes" ; then
	    if test "$flag" = " " -o "$flag" = "000" ; then 
		fflag="" 
	    else 
		fflag="$flag" 
	    fi
	    # discard options that don't allow mixed-case name matching
	    cat > conftest.f <<EOF
        program main
        call aB()
        end
        subroutine Ab()
        end
EOF
	    if test -n "$fflag" ; then flagval="with $fflag" ; else flagval="" ; fi
	    AC_MSG_CHECKING([whether Fortran 77 routine names are case-insensitive $flagval])
	    dnl we can use double quotes here because all is already
            dnl evaluated
            ac_fcompilelink_test="${F77-f77} -o conftest $fflag $FFLAGS conftest.f $LDFLAGS $LIBS 1>&AC_FD_CC"
	    if AC_TRY_EVAL(ac_fcompilelink_test) && test -x conftest ; then
	        AC_MSG_RESULT(yes)
	    else
	        AC_MSG_RESULT(no)
	        opt_ok="no"
            fi
        fi
        if test "$opt_ok" = "yes" ; then
	    trial_FLAGS="$trial_FLAGS
$flag"
        fi
    done
    IFS="$save_IFS"
    # Name of routines.  Since these are in groups, we use a case statement
    # and loop until the end (accomplished by reaching the end of the
    # case statement
    # For one version of Nag F90, the names are 
    # call f90_unix_MP_getarg(i,s) and f90_unix_MP_iargc().
    trial=0
    while test -z "$pac_cv_prog_f77_cmdarg" ; do
        case $trial in 
	0) # User-specified values, if any
	   if test -z "$F77_GETARG" -o -z "$F77_IARGC" ; then 
	       trial=`expr $trial + 1`
	       continue 
           fi
           MSG="Using environment values of F77_GETARG etc."
	   ;;
	1) # Standard practice, uppercase (some compilers are case-sensitive)
	   FXX_MODULE=""
	   F77_GETARGDECL="external GETARG"
	   F77_GETARG="call GETARG(i,s)"
	   F77_IARGC="IARGC()"
	   MSG="GETARG and IARGC"
	   ;;
	2) # Standard practice, lowercase
	   FXX_MODULE=""
           F77_GETARGDECL="external getarg"
	   F77_GETARG="call getarg(i,s)"
	   F77_IARGC="iargc()"
	   MSG="getarg and iargc"
	   ;;
	3) # Posix alternative
	   FXX_MODULE=""
	   F77_GETARGDECL="external pxfgetarg"
	   F77_GETARG="call pxfgetarg(i,s,l,ier)"
	   F77_IARGC="ipxfargc()"
	   MSG="pxfgetarg and ipxfargc"
	   ;;
	4) # Nag f90_unix_env module
	   FXX_MODULE="        use f90_unix_env"
	   F77_GETARGDECL=""
	   F77_GETARG="call getarg(i,s)"
	   F77_IARGC="iargc()"
	   MSG="f90_unix_env module"
	   ;;
        5) # Nag f90_unix module
	   FXX_MODULE="        use f90_unix"
	   F77_GETARGDECL=""
	   F77_GETARG="call getarg(i,s)"
	   F77_IARGC="iargc()"
	   MSG="f90_unix module"
	   ;;
	6) # user spec in a file
	   if test -s f77argdef ; then
		. ./f77argdef
	       MSG="Using definitions in the file f77argdef"
	   else
	        trial=`expr $trial + 1`
		continue
	   fi
	   ;;
	7) # gfortran won't find getarg if it is marked as external 
	   FXX_MODULE=""
	   F77_GETARGDECL="intrinsic GETARG"
	   F77_GETARG="call GETARG(i,s)"
	   F77_IARGC="IARGC()"
	   MSG="intrinsic GETARG and IARGC"
	   ;;
        *) # exit from while loop
	   FXX_MODULE=""
	   F77_GETARGDECL=""
	   F77_GETARG=""
	   F77_IARGC=""
           break
	   ;;
	esac
	# Create the program.  Make sure that we can run it.
	# Force a divide-by-zero if there is a problem (but only at runtime!
        # (the Absoft compiler does divide-by-zero at compile time)
        cat > conftest.f <<EOF
        program main
$FXX_MODULE
        integer i, j
        character*20 s
        $F77_GETARGDECL
        i = 0
        $F77_GETARG
        i=$F77_IARGC
        if (i .gt. 1) then
            j = i - $F77_IARGC
            j = 1.0 / j
        endif
        end
EOF
    #
    # Now, try to find some way to compile and link that program, looping 
    # over the possibilities of options and libraries
        save_IFS="$IFS"
        IFS=" ""
"
        for libs in $trial_LIBS ; do
            if test -n "$pac_cv_prog_f77_cmdarg" ; then break ; fi
	    if test "$libs" = " " -o "$libs" = "0" ; then libs="" ; fi
            for flags in $trial_FLAGS ; do
	        if test "$flags" = " " -o "$flags" = "000"; then flags="" ; fi
                AC_MSG_CHECKING([whether ${F77-f77} $flags $libs works with $MSG])
		IFS="$save_IFS"
		dnl We need this here because we've fiddled with IFS
	        ac_fcompilelink_test="${F77-f77} -o conftest $FFLAGS $flags conftest.f $LDFLAGS $libs $LIBS 1>&AC_FD_CC"
		found_answer="no"
                if AC_TRY_EVAL(ac_fcompilelink_test) && test -x conftest ; then
		    if test "$ac_cv_prog_f77_cross" != "yes" -a \	 
		            "$cross_compiling" != "yes" ; then
			if ./conftest >/dev/null 2>&1 ; then
			    found_answer="yes"
			fi
		    else 
			found_answer="yes"
		    fi
                fi
	        IFS=" ""
"
		if test "$found_answer" = "yes" ; then
	            AC_MSG_RESULT([yes])
		    pac_cv_prog_f77_cmdarg="$MSG"
		    pac_cv_prog_f77_cmdarg_fflags="$flags"
		    pac_cv_prog_f77_cmdarg_ldflags="$libs"
		    break
	        else
                    AC_MSG_RESULT([no])
		    echo "configure: failed program was:" >&AC_FD_CC
                    cat conftest.f >&AC_FD_CC
	        fi
            done
        done
        IFS="$save_IFS"   
	rm -f conftest.*
        trial=`expr $trial + 1`   
    done
fi
pac_cv_F77_GETARGDECL="$F77_GETARGDECL"
pac_cv_F77_IARGC="$F77_IARGC"
pac_cv_F77_GETARG="$F77_GETARG"
pac_cv_FXX_MODULE="$FXX_MODULE"
])
if test "$found_cached" = "yes" ; then 
    AC_MSG_RESULT([$pac_cv_prog_f77_cmdarg])
elif test -z "$pac_cv_F77_IARGC" ; then
    AC_MSG_WARN([Could not find a way to access the command line from Fortran 77])
fi
# Set the variable values based on pac_cv_prog_xxx
F77_GETARGDECL="$pac_cv_F77_GETARGDECL"
F77_IARGC="$pac_cv_F77_IARGC"
F77_GETARG="$pac_cv_F77_GETARG"
FXX_MODULE="$pac_cv_FXX_MODULE"
F77_GETARG_FFLAGS="$pac_cv_prog_f77_cmdarg_fflags"
F77_GETARG_LDFLAGS="$pac_cv_prog_f77_cmdarg_ldflags"
AC_SUBST(F77_GETARGDECL)
AC_SUBST(F77_IARGC)
AC_SUBST(F77_GETARG)
AC_SUBST(FXX_MODULE)
AC_SUBST(F77_GETARG_FFLAGS)
AC_SUBST(F77_GETARG_LDFLAGS)
])

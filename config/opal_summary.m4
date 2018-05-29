dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2016-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
AC_DEFUN([OPAL_SUMMARY_ADD],[
    OPAL_VAR_SCOPE_PUSH([ompi_summary_section ompi_summary_line ompi_summary_section_current])

    dnl need to replace spaces in the section name with somethis else. _ seems like a reasonable
    dnl choice. if this changes remember to change OMPI_PRINT_SUMMARY as well.
    ompi_summary_section=$(echo $1 | tr ' ' '_')
    ompi_summary_line="$2: $4"
    ompi_summary_section_current=$(eval echo \$ompi_summary_values_$ompi_summary_section)

    if test -z "$ompi_summary_section_current" ; then
        if test -z "$ompi_summary_sections" ; then
            ompi_summary_sections=$ompi_summary_section
        else
            ompi_summary_sections="$ompi_summary_sections $ompi_summary_section"
        fi
        eval ompi_summary_values_$ompi_summary_section=\"$ompi_summary_line\"
    else
        eval ompi_summary_values_$ompi_summary_section=\"$ompi_summary_section_current,$ompi_summary_line\"
    fi

    OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_SUMMARY_PRINT],[
    OPAL_VAR_SCOPE_PUSH([ompi_summary_section ompi_summary_section_name])
    cat <<EOF

Open MPI configuration:
-----------------------
Version: $OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION.$OMPI_RELEASE_VERSION$OMPI_GREEK_VERSION
EOF

    if test "$project_ompi_amc" = "true" ; then
        echo "Build MPI C bindings: yes"
    else
        echo "Build MPI C bindings: no"
    fi

    dnl Print out the bindings if we are building OMPI
    if test "$project_ompi_amc" = "true" ; then
	if test x$enable_mpi_cxx = xyes ; then
            echo "Build MPI C++ bindings (deprecated): yes"
	else
            echo "Build MPI C++ bindings (deprecated): no"
	fi

	if test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_MPIFH_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h"
	elif test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_USEMPI_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h, use mpi"
	elif test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_USEMPIF08_BINDINGS ; then
	    echo "Build MPI Fortran bindings: mpif.h, use mpi, use mpi_f08"
	else
	    echo "Build MPI Fortran bindings: no"
	fi

        if test $WANT_MPI_JAVA_BINDINGS -eq 1 ; then
            echo "Build MPI Java bindings (experimental): yes"
        else
            echo "MPI Build Java bindings (experimental): no"
        fi
    fi

    if test "$project_oshmem_amc" = "true" ; then
        echo "Build Open SHMEM support: yes"
    elif test -z "$project_oshmem_amc" ; then
        echo "Build Open SHMEM support: no"
    else
	echo "Build Open SHMEM support: $project_oshmem_amc"
    fi

    if test $WANT_DEBUG = 0 ; then
        echo "Debug build: no"
    else
        echo "Debug build: yes"
    fi

    if test ! -z $with_platform ; then
        echo "Platform file: $with_platform"
    else
        echo "Platform file: (none)"
    fi

    echo

    for ompi_summary_section in $(echo $ompi_summary_sections) ; do
        ompi_summary_section_name=$(echo $ompi_summary_section | tr '_' ' ')
        echo "$ompi_summary_section_name"
        echo "-----------------------"
        echo "$(eval echo \$ompi_summary_values_$ompi_summary_section)" | tr ',' $'\n' | sort -f
        echo " "
    done

    if test $WANT_DEBUG = 1 ; then
        cat <<EOF
*****************************************************************************
 THIS IS A DEBUG BUILD!  DO NOT USE THIS BUILD FOR PERFORMANCE MEASUREMENTS!
*****************************************************************************

EOF
    fi

    OPAL_VAR_SCOPE_POP
])

dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_GET_FORUM_ABI_VALUES],[
    AC_REQUIRE([AM_PATH_PYTHON])
    
    # Extract the MPI Forum ABI value of MPI_MAX_OBJECT_NAME from the
    # standard ABI JSON.  That JSON is the single source of truth: the
    # binding generator (ompi/mpi/bindings/c_header.py) consumes the
    # same file to emit MPI_MAX_OBJECT_NAME_ABI_INTERNAL into the
    # generated abi.h.  Open MPI sizes its internal object-name
    # storage (datatype/window name[] arrays and the communicator
    # c_name allocation) to this value so that the standard-ABI entry
    # points can store full-length names, while the traditional OMPI
    # entry points continue to honor the long-standing
    # OPAL_MAX_OBJECT_NAME.
    #
    # This must run *after* AM_PATH_PYTHON so that $PYTHON is
    # defined.  The Python uses .get() chaining rather than ['...']
    # subscripts so that there are no square brackets for m4 to
    # consume.

    AC_MSG_CHECKING([for the MPI Forum ABI MPI_MAX_OBJECT_NAME value])

    OMPI_MPI_MAX_OBJECT_NAME_ABI=`$PYTHON -c "import json; print(json.load(open('$srcdir/docs/mpi-standard-abi.json')).get('constants').get('mpi_max_object_name').get('abi_value'))"`
    AS_IF([test -z "$OMPI_MPI_MAX_OBJECT_NAME_ABI"],
          [AC_MSG_ERROR([Could not extract the MPI Forum ABI MPI_MAX_OBJECT_NAME value from docs/mpi-standard-abi.json])])
    AC_DEFINE_UNQUOTED([OMPI_MPI_MAX_OBJECT_NAME_ABI], [$OMPI_MPI_MAX_OBJECT_NAME_ABI],
                       [Maximum MPI object name length (in bytes) mandated by the MPI Forum ABI; Open MPI sizes its internal name storage to this value so the standard-ABI entry points can store full-length names])

    AC_MSG_RESULT([$OMPI_MPI_MAX_OBJECT_NAME_ABI])
])

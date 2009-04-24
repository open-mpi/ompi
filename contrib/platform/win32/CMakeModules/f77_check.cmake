# Copyright (c) 2008      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_F77_CHECK(Fortran type, c type required, 
#                types to search, expected size)
#----------------------------------------------------------
# Check Fortran type, including:
# - whether compiler supports or not
# - size of type
# - equal to expected size
# - alignment
# - associated C type
#
# types to search is a list of values

MACRO(OMPI_F77_CHECK FORTRAN_TYPE C_TYPE TYPE_LIST EXPECTED_SIZE)

  STRING(REPLACE "*" "" TYPE_NAME ${FORTRAN_TYPE})
  STRING(REPLACE " " "_" TYPE_NAME ${TYPE_NAME})
  SET(NEED_RECHECK TRUE)

  # do we need to check all the features?
  IF(DEFINED OMPI_HAVE_FORTRAN_${TYPE_NAME})
    IF(${F77_SETUP_${TYPE_NAME}} STREQUAL ${OMPI_WANT_F77_BINDINGS})
      SET(NEED_RECHECK FALSE)
    ELSE(${F77_SETUP_${TYPE_NAME}} STREQUAL ${OMPI_WANT_F77_BINDINGS})
      SET(NEED_RECHECK TRUE)
    ENDIF(${F77_SETUP_${TYPE_NAME}} STREQUAL ${OMPI_WANT_F77_BINDINGS})
  ENDIF(DEFINED OMPI_HAVE_FORTRAN_${TYPE_NAME})
  
  # use this variable to check whether user changed F77 option.
  # every time OMPI_WANT_F77_BINDINGS got changed, we need to re-check everything.
  SET(F77_SETUP_${TYPE_NAME} ${OMPI_WANT_F77_BINDINGS} CACHE INTERNAL "requir re-check ${TYPE_NAME} or not")

  IF(NEED_RECHECK)

    INCLUDE(F77_check_type)
    INCLUDE(F77_get_alignment)
    INCLUDE(F77_get_sizeof)
    INCLUDE(OMPI_find_type)

    SET(ofc_expected_size ${EXPECTED_SIZE})
    SET(ofc_have_type 0)
    SET(ofc_type_size ${SIZEOF_INT})
    SET(ofc_type_alignment ${SIZEOF_INT})
    SET(ofc_c_type ${ompi_fortran_bogus_type_t})

    # Only check if we actually want the F77 bindings / have a F77
    # compiler.  This allows us to call this macro, even if there is
    # no F77 compiler.  If we have no f77 compiler, then just set a
    # bunch of defaults.
    IF(OMPI_WANT_F77_BINDINGS)
      OMPI_F77_CHECK_TYPE(${FORTRAN_TYPE} ofc_have_type)
    ELSE(OMPI_WANT_F77_BINDINGS)
      # skip checking, set with expected values
      # MESSAGE(STATUS "Checking if Fortran 77 compiler supports ${FORTRAN_TYPE}...skipped.")
    ENDIF(OMPI_WANT_F77_BINDINGS)


    IF(ofc_have_type)
      # What is the size of this type?
      
      # NOTE: Some Fortran compilers actually will return that a
      # type exists even if it doesn't support it -- the compiler
      # will automatically convert the unsupported type to a type
      # that it *does* support.  For example, if you try to use
      # INTEGER*16 and the compiler doesn't support it, it may well
      # automatically convert it to INTEGER*8 for you (!).  So we
      # have to check the actual size of the type once we determine
      # that the compiler doesn't error if we try to use it
      # (i.e,. the compiler *might* support that type).  If the size
      # doesn't match the expected size, then the compiler doesn't
      # really support it.
      OMPI_F77_GET_SIZEOF(${FORTRAN_TYPE} ofc_type_size)
      
      IF(NOT ${ofc_expected_size} STREQUAL "-1" AND NOT ${ofc_type_size} EQUAL "${ofc_expected_size}")
        MESSAGE(STATUS "*** Fortran 77 ${FORTRAN_TYPE} does not have expected size!")
        MESSAGE(STATUS "*** Expected ${ofc_expected_size}, got ${ofc_type_size}")
        MESSAGE(STATUS "*** Disabling MPI support for Fortran 77 ${FORTRAN_TYPE}")
        SET(ofc_have_type 0)
      ELSE(NOT ${ofc_expected_size} STREQUAL "-1" AND NOT ${ofc_type_size} EQUAL "${ofc_expected_size}")
        # Look for a corresponding C type (will abort by itself if the
        # type isn't found and we need it)
        SET(ofc_c_type "")
        OMPI_FIND_TYPE(${FORTRAN_TYPE} "${TYPE_LIST}" ${C_TYPE} ${ofc_type_size} ofc_c_type)
        IF("${ofc_c_type}" STREQUAL "")
          SET(ofc_have_type 0)
        ENDIF("${ofc_c_type}" STREQUAL "")
        
        # Get the alignment of the type
        IF(ofc_have_type)
          OMPI_F77_GET_ALIGNMENT(${FORTRAN_TYPE} ofc_type_alignment)
        ENDIF(ofc_have_type)

      ENDIF(NOT ${ofc_expected_size} STREQUAL "-1" AND NOT ${ofc_type_size} EQUAL "${ofc_expected_size}")
    ENDIF(ofc_have_type)

    # We always need these defines -- even if we don't have a given type,
    # there are some places in the code where we have to have *something*.
    SET(OMPI_HAVE_FORTRAN_${TYPE_NAME} ${ofc_have_type} CACHE INTERNAL "OMPI_HAVE_FORTRAN_${TYPE_NAME}")
    SET(OMPI_SIZEOF_FORTRAN_${TYPE_NAME} ${ofc_type_size} CACHE INTERNAL "OMPI_SIZEOF_FORTRAN_${TYPE_NAME}")
    SET(OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME} ${ofc_type_alignment} CACHE INTERNAL "OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME}")
    IF(NOT "${TYPE_LIST}" STREQUAL "")
      STRING(TOLOWER ${TYPE_NAME} TYPE_NAME_L)
      SET(ompi_fortran_${TYPE_NAME_L}_t ${ofc_c_type} CACHE INTERNAL "ompi_fortran_${TYPE_NAME_L}_t")
    ENDIF(NOT "${TYPE_LIST}" STREQUAL "")

    #MESSAGE("OMPI_HAVE_FORTRAN_${TYPE_NAME}:${OMPI_HAVE_FORTRAN_${TYPE_NAME}}")
    #MESSAGE("OMPI_SIZEOF_FORTRAN_${TYPE_NAME}:${OMPI_SIZEOF_FORTRAN_${TYPE_NAME}}")
    #MESSAGE("OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME}:${OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME}}")
    #MESSAGE("ompi_fortran_${TYPE_NAME_L}_t:${ompi_fortran_${TYPE_NAME_L}_t}")

  ENDIF(NEED_RECHECK)

ENDMACRO(OMPI_F77_CHECK FORTRAN_TYPE C_TYPE TYPE_LIST EXPECTED_SIZE)

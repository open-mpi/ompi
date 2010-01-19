# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
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
  STRING(TOLOWER ${TYPE_NAME} TYPE_NAME_L)

  IF(OMPI_WANT_F77_BINDINGS AND NOT DEFINED F77_SETUP_${TYPE_NAME}_DONE)

    INCLUDE(F77_check_type)
    INCLUDE(F77_get_alignment)
    INCLUDE(F77_get_sizeof)
    INCLUDE(ompi_find_type)

    SET(ofc_expected_size ${EXPECTED_SIZE})
    SET(ofc_have_type 0)
    SET(ofc_type_size ${SIZEOF_INT})
    SET(ofc_type_alignment ${SIZEOF_INT})
    SET(ofc_c_type ${ompi_fortran_bogus_type_t})

    OMPI_F77_CHECK_TYPE(${FORTRAN_TYPE} ofc_have_type)

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
      SET(ompi_fortran_${TYPE_NAME_L}_t ${ofc_c_type} CACHE INTERNAL "ompi_fortran_${TYPE_NAME_L}_t")
    ENDIF(NOT "${TYPE_LIST}" STREQUAL "")

    SET(F77_SETUP_${TYPE_NAME}_DONE TRUE CACHE INTERNAL "F77 ${TYPE_NAME} check done or not.")

    #MESSAGE("OMPI_HAVE_FORTRAN_${TYPE_NAME}:${OMPI_HAVE_FORTRAN_${TYPE_NAME}}")
    #MESSAGE("OMPI_SIZEOF_FORTRAN_${TYPE_NAME}:${OMPI_SIZEOF_FORTRAN_${TYPE_NAME}}")
    #MESSAGE("OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME}:${OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME}}")
    #MESSAGE("ompi_fortran_${TYPE_NAME_L}_t:${ompi_fortran_${TYPE_NAME_L}_t}")

  ELSEIF(NOT OMPI_WANT_F77_BINDINGS)

    # when don't want F77 bindings, just set them to int.
    SET(OMPI_HAVE_FORTRAN_${TYPE_NAME} 0 CACHE INTERNAL "OMPI_HAVE_FORTRAN_${TYPE_NAME}")
    SET(ompi_fortran_${TYPE_NAME_L}_t int CACHE INTERNAL "ompi_fortran_${TYPE_NAME_L}_t")
    SET(OMPI_SIZEOF_FORTRAN_${TYPE_NAME} ${SIZEOF_INT} CACHE INTERNAL "OMPI_SIZEOF_FORTRAN_${TYPE_NAME}")
    SET(OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME} ${OPAL_ALIGNMENT_INT} CACHE INTERNAL "OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME}")

    UNSET(F77_SETUP_${TYPE_NAME}_DONE CACHE)

  ENDIF(OMPI_WANT_F77_BINDINGS AND NOT DEFINED F77_SETUP_${TYPE_NAME}_DONE)

OMPI_DEF_VAR(OMPI_HAVE_FORTRAN_${TYPE_NAME} "Whether we have Fortran 77 `${FORTRAN_TYPE}' or not." 0 1)
OMPI_DEF_VAR(OMPI_SIZEOF_FORTRAN_${TYPE_NAME} "Size of Fortran 77 `${FORTRAN_TYPE}'." 0 1)
OMPI_DEF_VAR(OMPI_ALIGNMENT_FORTRAN_${TYPE_NAME} "Alignment of Fortran 77 `${FORTRAN_TYPE}'." 0 1)
OMPI_DEF_VAR(ompi_fortran_${TYPE_NAME_L}_t "C type corresponding to Fortran 77 `${FORTRAN_TYPE}'." 0 1)

ENDMACRO(OMPI_F77_CHECK FORTRAN_TYPE C_TYPE TYPE_LIST EXPECTED_SIZE)

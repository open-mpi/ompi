.. _mpi_type_match_size:


MPI_Type_match_size
===================

.. include_body

:ref:`MPI_Type_match_size` |mdash| Returns an MPI datatype of a given type and
size

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_match_size.rst

INPUT PARAMETERS
----------------
* ``typeclass``: Generic type specifier (integer).
* ``size``: Size, in bytes, of representation (integer).

OUTPUT PARAMETERS
-----------------
* ``type``: Datatype with correct type and size (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function returns an MPI datatype matching a local variable of type
(*typeclass*, *size*). The returned type is a reference (handle) to a
predefined named datatype, not a duplicate. This type cannot be freed.

The value of *typeclass* may be set to one of MPI_TYPECLASS_REAL,
MPI_TYPECLASS_INTEGER, or MPI_TYPECLASS_COMPLEX, corresponding to the
desired datatype.

MPI_type_match_size can be used to obtain a size-specific type that
matches a Fortran numeric intrinsic type: first call :ref:`MPI_Sizeof` to
compute the variable size, then call :ref:`MPI_Type_match_size` to find a
suitable datatype. In C use the sizeof builtin instead of :ref:`MPI_Sizeof`.

It is erroneous to specify a size not supported by the compiler.


NOTES
-----

In Open MPI, *typeclass* always refers to a Fortran numeric intrinsic
type, and :ref:`MPI_Type_match_size` only ever returns an intrinsic
Fortran predefined datatype (for example, ``MPI_REAL8``,
``MPI_INTEGER4``, or ``MPI_COMPLEX8``) |mdash| regardless of whether it
is called from C or Fortran. It does not return C predefined datatypes
(such as ``MPI_DOUBLE``), nor composite predefined datatypes (such as
``MPI_2REAL`` or ``MPI_2INTEGER``), even when one of those happens to
have the requested *size*.

Consequently, if Open MPI was built with ``--disable-mpi-fortran``, the
Fortran intrinsic datatypes are unavailable. In that case no datatype
can match any *typeclass* / *size* combination: every such request is
treated as a size not supported by the compiler, and the call fails with
error class ``MPI_ERR_ARG`` (subject to the relevant error handler).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Sizeof`
   * :ref:`MPI_Type_get_extent`

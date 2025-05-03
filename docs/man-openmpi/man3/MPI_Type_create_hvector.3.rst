.. _mpi_type_create_hvector:


MPI_Type_create_hvector
=======================

.. include_body

:ref:`MPI_Type_create_hvector` |mdash| Creates a vector (strided) data type with offset in bytes.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_create_hvector.rst

INPUT PARAMETERS
----------------
* ``count``: Number of blocks (nonnegative integer).
* ``blocklength``: Number of elements in each block (nonnegative integer).
* ``stride``: Number of bytes between start of each block (integer).
* ``oldtype``: Old data type (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_hvector` creates a vector (strided) data type with offset
in bytes. This routine replaces :ref:`MPI_Type_hvector`, which is deprecated.

The function :ref:`MPI_Type_create_hvector` is identical to :ref:`MPI_Type_vector`, except
that stride is given in bytes, rather than in elements. The use for both
types of vector constructors is illustrated in the examples in the
Datatype chapter of the `MPI Standard
<https://www.mpi-forum.org/docs/>`_.

Assume that oldtype has type map

::

       {(type(0), disp(0)), ..., (type(n-1), disp(n-1))}

with extent ex. Let bl be the blocklength. The newly created datatype
has a type map with ``count * bl * n`` entries:

::

     {(type(0), disp(0)), ..., (type(n-1), disp(n-1)),
     (type(0), disp(0) + ex), ..., (type(n-1), disp(n-1) + ex),
     ..., (type(0), disp(0) + (bl -1) * ex),...,(type(n-1),
     disp(n-1) + (bl -1) * ex), (type(0), disp(0) + stride),
     ...,(type(n-1), disp(n-1) + stride), ..., (type(0),
     disp(0) + stride + (bl - 1) * ex), ..., (type(n-1),
     disp(n-1) + stride + (bl -1) * ex), ..., (type(0),
     disp(0) + stride * (count -1)), ...,(type(n-1),
     disp(n-1) + stride * (count -1)), ..., (type(0),
     disp(0) + stride * (count -1) + (bl -1) * ex), ...,
     (type(n-1), disp(n-1) + stride * (count -1) + (bl -1) * ex)}


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_hindexed`
   * :ref:`MPI_Type_vector`

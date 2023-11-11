.. _mpi_attr_put:


MPI_Attr_put
============

.. include_body

:ref:`MPI_Attr_put` |mdash| Stores attribute value associated with a key -- |deprecated_favor| :ref:`MPI_Comm_set_attr`.



SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_ATTR_PUT(COMM, KEYVAL, ATTRIBUTE_VAL, IERROR)
   	INTEGER	COMM, KEYVAL, ATTRIBUTE_VAL, IERROR


INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute will be attached (handle).
* ``keyval``: Key value, as returned by MPI_KEYVAL_CREATE (integer).
* ``attribute_val``: Attribute value.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated as of MPI-2, and* was
deleted in MPI-3. Please use :ref:`MPI_Comm_set_attr`. This function does not
have a mpi_f08 binding.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_set_attr`

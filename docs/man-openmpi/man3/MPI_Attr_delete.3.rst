.. _mpi_attr_delete:


MPI_Attr_delete
===============

.. include_body

:ref:`MPI_Attr_delete` |mdash| Deletes attribute value associated with a key |mdash| |deprecated_favor| :ref:`MPI_Comm_delete_attr`.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Attr_delete(MPI_Comm comm, int keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_ATTR_DELETE(COMM, KEYVAL, IERROR)
   	INTEGER	COMM, KEYVAL, IERROR


INPUT PARAMETERS
----------------
* ``comm``: Communicator to which attribute is attached (handle).
* ``keyval``: The key value of the deleted attribute (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2, and was
*deleted* in MPI-3. Please use :ref:`MPI_Comm_delete_attr`. This function does
not have a mpi_f08 binding.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_delete_attr`

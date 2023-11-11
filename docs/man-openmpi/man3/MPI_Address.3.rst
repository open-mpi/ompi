.. _mpi_address:


MPI_Address
===========

.. include_body

:ref:`MPI_Address` |mdash| Gets the address of a location in memory -- |deprecated_favor| :ref:`MPI_Get_address`.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Address(void *location, MPI_Aint *address)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_ADDRESS(LOCATION, ADDRESS, IERROR)
   	<type>	LOCATION (*)
   	INTEGER	ADDRESS, IERROR


INPUT PARAMETER
---------------
* ``location``: Location in caller memory (choice).

OUTPUT PARAMETERS
-----------------
* ``address``: Address of location (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Get_address` instead.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Get_address`

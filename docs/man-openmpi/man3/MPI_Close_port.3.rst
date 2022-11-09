.. _mpi_close_port:

MPI_Close_port
==============

.. include_body

:ref:`MPI_Close_port` - Releases the specified network address.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Close_port(const char *port_name)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CLOSE_PORT(PORT_NAME, IERROR)
       CHARACTER*(*)   PORT_NAME
       INTEGER     IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Close_port(port_name, ierror)
       CHARACTER(LEN=*), INTENT(IN) :: port_name
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  ``port_name`` : A port (string).

Output Parameter
----------------

-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Close_port` releases the network address represented by
``port_name``.

Errors
------

.. include:: ./ERRORS.rst

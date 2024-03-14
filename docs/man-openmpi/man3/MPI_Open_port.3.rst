.. _mpi_open_port:


MPI_Open_port
=============

.. include_body

:ref:`MPI_Open_port` |mdash| Establishes a network address for a server to accept
connections from clients.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Open_port(MPI_Info info, char *port_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_OPEN_PORT(INFO, PORT_NAME, IERROR)
   	CHARACTER*(*)	PORT_NAME
   	INTEGER		INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Open_port(info, port_name, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``info``: Options on how to establish an address (handle). No options currently supported.

OUTPUT PARAMETERS
-----------------
* ``port_name``: Newly established port (string).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Open_port` establishes a network address, encoded in the *port_name*
string, at which the server will be able to accept connections from
clients. *port_name* is supplied by the system.

MPI copies a system-supplied port name into *port_name*. *port_name*
identifies the newly opened port and can be used by a client to contact
the server. The maximum size string that may be supplied by the system
is MPI_MAX_PORT_NAME.


SUPPORTED INFO KEYS
-------------------

None.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_accept`
   * :ref:`MPI_Comm_connect`

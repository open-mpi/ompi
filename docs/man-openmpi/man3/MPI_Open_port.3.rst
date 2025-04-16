.. _mpi_open_port:


MPI_Open_port
=============

.. include_body

:ref:`MPI_Open_port` |mdash| Establishes a network address for a server to accept
connections from clients.

.. The following file was automatically generated
.. include:: ./bindings/mpi_open_port.rst

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

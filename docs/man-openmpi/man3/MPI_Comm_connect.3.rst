.. _mpi_comm_connect:

MPI_Comm_connect
================

.. include_body

:ref:`MPI_Comm_connect` |mdash| Establishes communication with a server.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_connect.rst

INPUT PARAMETERS
----------------

* ``port_name`` : Port name (string, used only on *root*).
* ``info`` : Options given by root for the connect (handle, used only on
   root). No options currently supported.
* ``root`` : Rank in *comm* of root node (integer).
* ``comm`` : Intracommunicator over which call is collective (handle).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : Intercommunicator with client as remote group (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_connect` establishes communication with a server specified by
port_name. It is collective over the calling communicator and returns an
intercommunicator in which the remote group participated in an
:ref:`MPI_Comm_accept`. The :ref:`MPI_Comm_connect` call must only be called after the
:ref:`MPI_Comm_accept` call has been made by the MPI job acting as the server.
If the named port does not exist (or has been closed), :ref:`MPI_Comm_connect`
raises an error of class MPI_ERR_PORT. MPI provides no guarantee of
fairness in servicing connection attempts. That is, connection attempts
are not necessarily satisfied in the order in which they were initiated,
and competition from other connection attempts may prevent a particular
connection attempt from being satisfied. The port_name parameter is the
address of the server. It must be the same as the name returned by
:ref:`MPI_Open_port` on the server.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_accept`

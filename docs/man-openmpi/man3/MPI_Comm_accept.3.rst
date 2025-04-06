.. _mpi_comm_accept:

MPI_Comm_accept
===============

.. include_body

:ref:`MPI_Comm_accept` |mdash| Establishes communication with a client.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_accept.rst

INPUT PARAMETERS
----------------

* ``port_name`` : Port name (string, used only on *root*).
* ``info`` : Options given by root for the accept (handle, used only on
   root). No options currently supported.
* ``root`` : Rank in *comm* of root node (integer).
* ``comm`` : Intracommunicator over which call is collective (handle).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : Intercommunicator with client as remote group (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_accept` establishes communication with a client. It is
collective over the calling communicator. It returns an
intercommunicator that allows communication with the client, after the
client has connected with the :ref:`MPI_Comm_accept` function using the
:ref:`MPI_Comm_connect` function. The port_name must have been established
through a call to :ref:`MPI_Open_port` on the root.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_connect`

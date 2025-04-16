.. _mpi_close_port:

MPI_Close_port
==============

.. include_body

:ref:`MPI_Close_port` |mdash| Releases the specified network address.

.. The following file was automatically generated
.. include:: ./bindings/mpi_close_port.rst

INPUT PARAMETER
---------------

* ``port_name`` : A port (string).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Close_port` releases the network address represented by
``port_name``.

ERRORS
------

.. include:: ./ERRORS.rst

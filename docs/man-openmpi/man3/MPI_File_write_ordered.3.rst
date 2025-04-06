.. _mpi_file_write_ordered:

MPI_File_write_ordered
======================

.. include_body

:ref:`MPI_File_write_ordered` |mdash| Writes a file at a location specified by a
shared file pointer (blocking, collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_write_ordered.rst

INPUT PARAMETERS
----------------

* ``fh`` : File handle (handle).
* ``buf`` : Initial address of buffer (choice).
* ``count`` : Number of elements in buffer (integer).
* ``datatype`` : Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------

* ``status`` : Status object (Status).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_ordered` is a collective routine. This routine must be
called by all processes in the communicator group associated with the
file handle ``fh``. Each process may pass different argument values for
the ``datatype`` and ``count`` arguments. Each process attempts to
write, into the file associated with ``fh``, a total number of ``count``
data items having ``datatype`` type contained in the user's buffer
``buf``. For each process, the location in the file at which data is
written is the position at which the shared file pointer would be after
all processes whose ranks within the group are less than that of this
process had written their data. :ref:`MPI_File_write_ordered` returns the
number of ``datatype`` elements written in ``status``. The shared file
pointer is updated by the amounts of data requested by all processes of
the group.

ERRORS
------

.. include:: ./ERRORS.rst

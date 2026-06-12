.. _mpi_file_set_info:


MPI_File_set_info
=================

.. include_body

:ref:`MPI_File_set_info` |mdash| Sets new values for hints (collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_set_info.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETER
---------------
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_set_info` is a collective routine that sets new values
for the hints of the file associated with *fh*. These hints are set
for each file, using the :ref:`MPI_File_open`, :ref:`MPI_File_delete`,
:ref:`MPI_File_set_view`, and :ref:`MPI_File_set_info` routines. The
opaque *info* object, which allows you to provide hints for
optimization of your code, may be different on each process, but some
*info* entries are required to be the same on all processes: In these
cases, they must appear with the same value in each process's info
object. See the :ref:`HINTS section <man-openmpi-mpi-file-set-info>`
for a list of hints that can be set.

.. _man-openmpi-mpi-file-set-info:

HINTS
-----

.. include:: /tuning-apps/_include/ompio-mpi-info-hints.rst


ERRORS
------

.. include:: ./ERRORS.rst

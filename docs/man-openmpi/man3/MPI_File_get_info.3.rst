.. _mpi_file_get_info:


MPI_File_get_info
=================

.. include_body

:ref:`MPI_File_get_info` |mdash| Returns a new info object containing values for
current hints associated with a file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_info.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``info_used``: New info object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_info` returns a new info object containing all the hints
that the system currently associates with the file *fh*. The current
setting of all hints actually used by the system related to this open
file is returned in *info_used*. The user is responsible for freeing
*info_used* via :ref:`MPI_Info_free`.

Note that the set of hints returned in *info_used* may be greater or
smaller than the set of hints passed in to :ref:`MPI_File_open`,
:ref:`MPI_File_set_view`, and :ref:`MPI_File_set_info`, as the system
may not recognize some hints set by the user, and may automatically
set other hints that the user has not requested to be set. See the
:ref:`HINTS section <man-openmpi-mpi-file-get-info>` for a list of
hints that can be set.

.. _man-openmpi-mpi-file-get-info:


HINTS
-----

.. include:: /tuning-apps/_include/ompio-mpi-info-hints.rst



ERRORS
------

.. include:: ./ERRORS.rst

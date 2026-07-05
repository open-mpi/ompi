.. _mpi_file_set_view:


MPI_File_set_view
=================

.. include_body

:ref:`MPI_File_set_view` |mdash| Changes process's view of data in file
(collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_set_view.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``disp``: Displacement (integer).
* ``etype``: Elementary data type (handle).
* ``filetype``: File type (handle). See Restrictions, below.
* ``datarep``: Data representation (string).
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The :ref:`MPI_File_set_view` routine changes the process's view of the data in
the file |mdash| the beginning of the data accessible in the file through
that view is set to *disp;* the type of data is set to *etype;* and the
distribution of data to processes is set to *filetype.* In addition,
:ref:`MPI_File_set_view` resets the independent file pointers and the shared
file pointer to zero. :ref:`MPI_File_set_view` is collective across the *fh*;
all processes in the group must pass identical values for *datarep* and
provide an *etype* with an identical extent. The values for *disp*,
*filetype*, and *info* may vary. It is erroneous to use the shared file
pointer data-access routines unless identical values for *disp* and
*filetype* are also given. The data types passed in *etype* and
*filetype* must be committed.

The *disp* displacement argument specifies the position (absolute offset
in bytes from the beginning of the file) where the view begins.

The :ref:`MPI_File_set_view` interface allows the user to pass a
data-representation string to MPI I/O via the *datarep* argument. To
obtain the default value pass the value "native". The user can also
pass information via the *info* argument. See the :ref:`HINTS section
<man-openmpi-mpi-file-set-view>` for a list of hints that can be
set.

.. _man-openmpi-mpi-file-set-view:

HINTS
-----

.. include:: /tuning-apps/_include/ompio-mpi-info-hints.rst


ERRORS
------

.. include:: ./ERRORS.rst

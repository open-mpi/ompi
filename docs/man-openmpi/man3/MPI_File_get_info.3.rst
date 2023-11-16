.. _mpi_file_get_info:


MPI_File_get_info
=================

.. include_body

:ref:`MPI_File_get_info` |mdash| Returns a new info object containing values for
current hints associated with a file.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_info(MPI_File fh, MPI_Info *info_used)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_INFO(FH, INFO_USED, IERROR)
   	INTEGER	FH, INFO_USED, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_info(fh, info_used, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(MPI_Info), INTENT(OUT) :: info_used
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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

The following hints can be used as values for the *info_used* argument.

**SETTABLE HINTS**

* ``shared_file_timeout``: Amount of time (in seconds) to wait for
  access to the shared file pointer before exiting with
  ``MPI_ERR_TIMEDOUT``.

* ``rwlock_timeout``: Amount of time (in seconds) to wait for
  obtaining a read or write lock on a contiguous chunk of a UNIX file
  before exiting with ``MPI_ERR_TIMEDOUT``.

* ``noncoll_read_bufsize``: Maximum size of the buffer used by MPI I/O
  to satisfy read requests in the noncollective data-access
  routines.

  .. note:: A buffer size smaller than the distance (in bytes) in a
            UNIX file between the first byte and the last byte of the
            access request causes MPI I/O to iterate and perform
            multiple UNIX ``read()`` or ``write()`` calls. If the
            request includes multiple noncontiguous chunks of data,
            and the buffer size is greater than the size of those
            chunks, then the UNIX ``read()`` or ``write()`` (made at
            the MPI I/O level) will access data not requested by this
            process in order to reduce the total number of ``write()``
            calls made. If this is not desirable behavior, you should
            reduce this buffer size to equal the size of the
            contiguous chunks within the aggregate request.

* ``noncoll_write_bufsize``: Maximum size of the buffer used by MPI
  I/O to satisfy write requests in the noncollective data-access
  routines.

  See the above note in ``noncoll_read_bufsize``.

* ``coll_read_bufsize``: Maximum size of the buffer used by MPI I/O to
  satisfy read requests in the collective data-access routines.

  See the above note in ``noncoll_read_bufsize``.

* ``coll_write_bufsize``: Maximum size of the buffer used by MPI I/O
  to satisfy write requests in the collective data-access
  routines.

  See the above note in ``noncoll_read_bufsize``.

* ``mpiio_concurrency``: (boolean) controls whether nonblocking
  I/O routines can bind an extra thread to an LWP.

* ``mpiio_coll_contiguous``: (boolean) controls whether subsequent
  collective data accesses will request collectively contiguous
  regions of the file.

**NON-SETTABLE HINTS**

* ``filename``: Access this hint to get the name of the file.



ERRORS
------

.. include:: ./ERRORS.rst

.. _mpi_file_open:


MPI_File_open
=============

.. include_body

:ref:`MPI_File_open` |mdash| Opens a file (collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_open.rst

INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).
* ``filename``: Name of file to open (string).
* ``amode``: File access mode (integer).
* ``info``: Info object (handle).

OUTPUT PARAMETERS
-----------------
* ``fh``: New file handle (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_open` opens the file identified by the filename
*filename* on all processes in the *comm* communicator
group. :ref:`MPI_File_open` is a collective routine; all processes
must provide the same value for *amode,* and all processes must
provide filenames that reference the same file which are textually
identical (note: Open MPI I/O plugins may have restrictions on
characters that can be used in filenames.
A process can open a file independently of other processes
by using the MPI_COMM_SELF communicator. The file handle returned,
*fh,* can be subsequently used to access the file until the file is
closed using :ref:`MPI_File_close`. Before calling
:ref:`MPI_Finalize`, the user is required to close (via
MPI_File_close) all files that were opened with
:ref:`MPI_File_open`. Note that the communicator *comm* is unaffected
by :ref:`MPI_File_open` and continues to be usable in all MPI
routines. Furthermore, use of *comm* will not interfere with I/O
behavior.

Initially, all processes view the file as a linear byte stream; that is,
the *etype* and *filetype* are both MPI_BYTE. The file view can be
changed via the :ref:`MPI_File_set_view` routine.

The following access modes are supported (specified in amode, in a
bit-vector OR in one of the following integer constants):

* ``MPI_MODE_APPEND``
* ``MPI_MODE_CREATE``: Create the file if it does not exist.
* ``MPI_MODE_DELETE_ON_CLOSE``
* ``MPI_MODE_EXCL``: Error creating a file that already exists.
* ``MPI_MODE_RDONLY``: Read only.
* ``MPI_MODE_RDWR``: Reading and writing.
* ``MPI_MODE_SEQUENTIAL``
* ``MPI__MODE_WRONLY``: Write only.
* ``MPI_MODE_UNIQUE_OPEN``

The modes ``MPI_MODE_RDONLY``, ``MPI_MODE_RDWR``, ``MPI_MODE_WRONLY``,
and ``MPI_MODE_CREATE`` have identical semantics to their POSIX
counterparts. It is erroneous to specify ``MPI_MODE_CREATE`` in
conjunction with ``MPI_MODE_RDONLY``. Errors related to the access
mode are raised in the class ``MPI_ERR_AMODE``.

On single-node clusters, files are opened by default using nonatomic
mode file consistency semantics. The more stringent atomic-mode
consistency semantics, required for atomicity of overlapping accesses,
are the default when processors in a communicator group reside on more
than one node. This setting can be changed using :ref:`MPI_File_set_atomicity`.

The :ref:`MPI_File_open` interface allows the user to pass information
via the *info* argument. It can be set to ``MPI_INFO_NULL``. See the
:ref:`HINTS section <man-openmpi-mpi-file-open-hints>` for a list of
hints that can be set.

.. _man-openmpi-mpi-file-open-hints:

HINTS
-----

The following hints can be used as values for the *info* argument.

**SETTABLE HINTS**

* ``MPI_INFO_NULL``

* ``shared_file_timeout``: Amount of time (in seconds) to wait for
  access to the shared file pointer before exiting with
  ``MPI_ERR_TIMEDOUT``.

* ``rwlock_timeout``: Amount of time (in seconds) to wait for
  obtaining a read or write lock on a contiguous chunk of a UNIX file
  before exiting with ``MPI_ERR_TIMEDOUT``.

* ``noncoll_read_bufsize``: Maximum size of the buffer used by MPI I/O
  to satisfy multiple noncontiguous read requests in the noncollective
  data-access routines.

  .. note:: A buffer size smaller than the distance (in bytes) in a
            UNIX file between the first byte and the last byte of the
            access request causes MPI I/O to iterate and perform
            multiple UNIX `read()` or `write()` calls. If the request
            includes multiple noncontiguous chunks of data, and the
            buffer size is greater than the size of those chunks, then
            the UNIX `read()` or `write()` (made at the MPI I/O level)
            will access data not requested by this process in order to
            reduce the total number of `write()` calls made. If this
            is not desirable behavior, you should reduce this buffer
            size to equal the size of the contiguous chunks within the
            aggregate request.

* ``noncoll_write_bufsize``: Maximum size of the buffer used by MPI
  I/O to satisfy multiple noncontiguous write requests in the
  noncollective data-access routines.

  See the above note in ``noncoll_read_bufsize``.

* ``coll_read_bufsize``: Maximum size of the buffer used by MPI I/O to
  satisfy multiple noncontiguous read requests in the collective
  data-access routines.

  See the above note in ``noncoll_read_bufsize``.

* ``coll_write_bufsize``: Maximum size of the buffer used by MPI I/O
  to satisfy multiple noncontiguous write requests in the collective
  data-access routines.

  See the above note in ``noncoll_read_bufsize``.

* ``mpiio_concurrency``: (boolean) controls whether nonblocking I/O
  routines can bind an extra thread to an LWP.  .sp

* ``mpiio_coll_contiguous``: (boolean) controls whether subsequent
  collective data accesses will request collectively contiguous
  regions of the file.

**NON-SETTABLE HINTS**

* ``filename``: Access this hint to get the name of the file.


ERRORS
------

.. include:: ./ERRORS.rst

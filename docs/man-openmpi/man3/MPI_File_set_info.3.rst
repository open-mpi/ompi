.. _mpi_file_set_info:


MPI_File_set_info
=================

.. include_body

:ref:`MPI_File_set_info` |mdash| Sets new values for hints (collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_set_info(MPI_File fh, MPI_Info info)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_SET_INFO(FH, INFO, IERROR)
   	INTEGER	FH, INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_set_info(fh, info, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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

The following hints can be used as values for the *info* argument.

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
	    multiple UNIX ``read()`` or ``write()`` calls. If the request
	    includes multiple noncontiguous chunks of data, and the
	    buffer size is greater than the size of those chunks, then
	    the UNIX ``read()`` or ``write()`` (made at the MPI I/O level)
	    will access data not requested by this process in order to
	    reduce the total number of ``write()`` calls made. If this is
	    not desirable behavior, you should reduce this buffer size
	    to equal the size of the contiguous chunks within the
	    aggregate request.

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

* ``mpiio_concurrency``: (boolean) controls whether nonblocking I/O
  routines can bind an extra thread to an LWP.

* ``mpiio_coll_contiguous``: (boolean) controls whether subsequent
  collective data accesses will request collectively contiguous
  regions of the file.


  **NON-SETTABLE HINTS**

* ``filename``: Access this hint to get the name of the file.


ERRORS
------

.. include:: ./ERRORS.rst

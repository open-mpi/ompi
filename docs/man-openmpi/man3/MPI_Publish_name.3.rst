.. _mpi_publish_name:


MPI_Publish_name
================

.. include_body

:ref:`MPI_Publish_name` |mdash| Publishes a service name associated with a port


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Publish_name(const char *service_name, MPI_Info info,
   	const char *port_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PUBLISH_NAME(SERVICE_NAME, INFO, PORT_NAME, IERROR)
   	CHARACTER*(*)	SERVICE_NAME, PORT_NAME
   	INTEGER		INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Publish_name(service_name, info, port_name, ierror)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``service_name``: A service name (string).
* ``info``: Options to the name service functions (handle).
* ``port_name``: A port name (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine publishes the pair (*service_name, port_name*) so that an
application may retrieve *port_name* by calling :ref:`MPI_Lookup_name` with
*service_name* as an argument. It is an error to publish the same
*service_name* twice, or to use a *port_name* argument that was not
previously opened by the calling process via a call to :ref:`MPI_Open_port`.


INFO ARGUMENTS
--------------

The following keys for *info* are recognized:

::

   Key                   Type      Description
   ---                   ----      -----------

   range                 char *    Scope in which to publish the service
                                   name.  See the NAME SCOPE section
                                   below.

   persistence           char *    How long the published service name is
                                   retained.  See below.

The *range* info key accepts one of two string values:

*nspace*: Restrict the operation to processes in the same MPI job (PMIx
   namespace) as the calling process.

*session*: Apply the operation across all processes in the same session.

If the *info* argument is ``MPI_INFO_NULL``, or is a valid info object
that does not contain a *range* key, *session* scope is used. Because
:ref:`MPI_Publish_name`, :ref:`MPI_Lookup_name`, and
:ref:`MPI_Unpublish_name` all share this default, a name published with
``MPI_INFO_NULL`` is found by a lookup with ``MPI_INFO_NULL``.

Any other value for *range* results in an error.

The *persistence* info key accepts one of four string values:

*indef*: Retain the service name until it is explicitly unpublished.

*proc*: Retain the service name until the publishing process terminates.

*app*: Retain the service name until the publishing application
   terminates.

*session*: Retain the service name until the session terminates. This is
   the default if the *persistence* key is not given.

Any other value for *persistence* results in an error.


NAME SCOPE
----------

Open MPI supports two name scopes, selected by the *range* info key:
*nspace* and *session*.

*nspace* scope restricts the (service_name, port_name) pair to processes
in the publisher's own MPI job, i.e., processes sharing the publisher's
PMIx namespace.

*session* scope makes the pair visible to every process in the same PMIx
session. This includes jobs started by separate ``mpirun`` invocations
that share a persistent DVM or scheduler allocation, as well as jobs
created via :ref:`MPI_Comm_spawn`. *session* is the default scope.

The same scope must be used to publish, look up, and unpublish a given
service name. :ref:`MPI_Unpublish_name` returns an error if the service
name is not found in the indicated scope.

As an example of the impact of these scoping rules, consider a job
started with ``mpirun`` -- call it "job1". A process in job1 publishes a
service/port pair using *nspace* scope. A process in a job that job1
subsequently starts via :ref:`MPI_Comm_spawn` -- call it "job2" -- is in
a different namespace, and so cannot retrieve that pair. Had job1
published with *session* scope (the default), processes in job2 could
retrieve it, as could processes in any other job sharing the same
session.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Lookup_name`
   * :ref:`MPI_Open_port`

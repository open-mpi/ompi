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

   ompi_global_scope     bool      If set to true, publish the name in
                                   the global scope.  Publish in the local
                                   scope otherwise.  See the NAME SCOPE
                                   section for more details.

   ompi_unique           bool      If set to true, return an error if the
                                   specified service_name already exists.
                                   Default to overwriting any pre-existing
                                   value.

*bool* info keys are actually strings but are evaluated as follows: if
the string value is a number, it is converted to an integer and cast to
a boolean (meaning that zero integers are false and non-zero values are
true). If the string value is (case-insensitive) "yes" or "true", the
boolean is true. If the string value is (case-insensitive) "no" or
"false", the boolean is false. All other string values are unrecognized,
and therefore false.

If no info key is provided, the function will first check to see if a
global server has been specified and is available. If so, then the
publish function will default to global scope first, followed by local.
Otherwise, the data will default to publish with local scope.


NAME SCOPE
----------

Open MPI supports two name scopes: *global* and *local*. Local scope
will place the specified service/port pair in a data store located on
the mpirun of the calling process' job. Thus, data published with local
scope will only be accessible to processes in jobs spawned by that
mpirun - e.g., processes in the calling process' job, or in jobs spawned
via :ref:`MPI_Comm_spawn`.

Global scope places the specified service/port pair in a data store
located on a central server that is accessible to all jobs running in
the cluster or environment. Thus, data published with global scope can
be accessed by multiple mpiruns and used for :ref:`MPI_Comm_Connect` and
:ref:`MPI_Comm_accept` between jobs.

Note that global scope operations require both the presence of the
central server and that the calling process be able to communicate to
that server. :ref:`MPI_Publish_name` will return an error if global scope is
specified and a global server is either not specified or cannot be
found.

Open MPI provides a server called *ompi-server* to support global scope
operations. Please refer to its manual page for a more detailed
description of data store/lookup operations.

As an example of the impact of these scoping rules, consider the case
where a job has been started with mpirun - call this job "job1". A
process in job1 creates and publishes a service/port pair using a local
scope. Open MPI will store this data in the data store within mpirun.

A process in job1 (perhaps the same as did the publish, or perhaps some
other process in the job) subsequently calls :ref:`MPI_Comm_spawn` to start
another job (call it "job2") under this mpirun. Since the two jobs share
a common mpirun, both jobs have access to local scope data. Hence, a
process in job2 can perform an :ref:`MPI_Lookup_name` with a local scope to
retrieve the information.

However, assume another user starts a job using mpirun - call this job
"job3". Because the service/port data published by job1 specified local
scope, processes in job3 cannot access that data. In contrast, if the
data had been published using global scope, then any process in job3
could access the data, provided that mpirun was given knowledge of how
to contact the central server and the process could establish
communication with it.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Lookup_name`
   * :ref:`MPI_Open_port`

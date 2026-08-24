.. _mpi_lookup_name:


MPI_Lookup_name
===============

.. include_body

::

   MPI_Lookup_name - Finds port associated with a service name


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Lookup_name(const char *service_name, MPI_Info info,
   	char *port_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_LOOKUP_NAME(SERVICE_NAME, INFO, PORT_NAME, IERROR)
   	CHARACTER*(*)	SERVICE_NAME, PORT_NAME
   	INTEGER		INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Lookup_name(service_name, info, port_name, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: service_name
   	TYPE(MPI_Info), INTENT(IN) :: info
   	CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``service_name``: A service name (string).
* ``info``: Options to the name service functions (handle).

OUTPUT PARAMETERS
-----------------
* ``port_name``: a port name (string).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function retrieves a *port_name* published under *service_name* by
a previous invocation of :ref:`MPI_Publish_name`. The application must supply a
*port_name* buffer large enough to hold the largest possible port name
(i.e., MPI_MAX_PORT_NAME bytes).


INFO ARGUMENTS
--------------

The following keys for *info* are recognized:

::

   Key                   Type      Description
   ---                   ----      -----------

   range                 char *    Scope in which to search for the
                                   service name.  See the NAME SCOPE
                                   section below.

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

For a more detailed description of scoping rules, please see the
:ref:`MPI_Publish_name` man page.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Publish_name`
   * :ref:`MPI_Open_port`

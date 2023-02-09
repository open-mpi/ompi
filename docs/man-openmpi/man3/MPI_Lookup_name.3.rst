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

   ompi_lookup_order     char *    Resolution order for name lookup.

The *ompi_lookup_order* info key can specify one of four valid string
values (see the NAME SCOPE section below for more information on name
scopes):

*local*: Only search the local scope for name resolution.

*global*: Only search the global scope for name resolution.

*local,global*: Search the local scope for name resolution. If
   not found, try searching the global scope for name resolution. This
   behavior is the default if the *ompi_lookup_order* info key is not
   specified.

*global,local*: Search the global scope for name resolution. If
   not found, try searching the local scope for name resolution.

If no info key is provided, the search will first check to see if a
global server has been specified and is available. If so, then the
search will default to global scope first, followed by local. Otherwise,
the search will default to local.


NAME SCOPE
----------

Open MPI supports two name scopes: *global* and *local*. Local scope
values are placed in a data store located on the mpirun of the calling
process' job, while global scope values reside on a central server.
Calls to :ref:`MPI_Unpublish_name` must correctly specify the scope to be used
in finding the value to be removed. The function will return an error if
the specified service name is not found on the indicated location.

For a more detailed description of scoping rules, please see the
:ref:`MPI_Publish_name` man page.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Publish_name`
   * :ref:`MPI_Open_port`

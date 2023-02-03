.. _mpi_unpublish_name:


MPI_Unpublish_name
==================

.. include_body

::

   MPI_Unpublish_name - Unpublishes a service name


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Unpublish_name(const char *service_name, MPI_Info info,
   	const char *port_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_UNPUBLISH_NAME(SERVICE_NAME, INFO, PORT_NAME, IERROR)
   	CHARACTER*(*)	SERVICE_NAME, PORT_NAME
   	INTEGER		INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Unpublish_name(service_name, info, port_name, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
   	TYPE(MPI_Info), INTENT(IN) :: info
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

This routine removes the pair (*service_name, port_name*) so that
applications may no longer retrieve *port_name* by calling
:ref:`MPI_Lookup_name`. It is an error to unpublish a *service_name* that was
not published via :ref:`MPI_Publish_name`. Both the *service_name* and
*port_name* arguments to :ref:`MPI_Unpublish_name` must be identical to the
arguments to the previous call to :ref:`MPI_Publish_name`.


INFO ARGUMENTS
--------------

The following keys for *info* are recognized:

::

   Key                   Type      Description
   ---                   ----      -----------

   ompi_global_scope     bool      If set to true, unpublish the name from
                                   the global scope.  Unpublish from the local
                                   scope otherwise.  See the NAME SCOPE
                                   section for more details.

*bool* info keys are actually strings but are evaluated as follows: if
the string value is a number, it is converted to an integer and cast to
a boolean (meaning that zero integers are false and non-zero values are
true). If the string value is (case-insensitive) "yes" or "true", the
boolean is true. If the string value is (case-insensitive) "no" or
"false", the boolean is false. All other string values are unrecognized,
and therefore false.

If no info key is provided, the function will first check to see if a
global server has been specified and is available. If so, then the
unpublish function will default to global scope first, followed by
local. Otherwise, the data will default to unpublish with local scope.


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
   * :ref:`MPI_Lookup_name`
   * :ref:`MPI_Open_port`

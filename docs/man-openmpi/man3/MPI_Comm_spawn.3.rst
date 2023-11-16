.. _mpi_comm_spawn:


MPI_Comm_spawn
==============

.. include_body

:ref:`MPI_Comm_spawn` |mdash| Spawns a number of identical binaries.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_spawn(const char *command, char *argv[], int maxprocs,
   	MPI_Info info, int root, MPI_Comm comm,
   	MPI_Comm *intercomm, int array_of_errcodes[])


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SPAWN(COMMAND, ARGV, MAXPROCS, INFO, ROOT, COMM,
   	INTERCOMM, ARRAY_OF_ERRCODES, IERROR)

   	CHARACTER*(*) COMMAND, ARGV(*)
   	INTEGER	INFO, MAXPROCS, ROOT, COMM, INTERCOMM,
   	ARRAY_OF_ERRCODES(*), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_spawn(command, argv, maxprocs, info, root, comm, intercomm,
   		array_of_errcodes, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: command, argv(*)
   	INTEGER, INTENT(IN) :: maxprocs, root
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   	INTEGER :: array_of_errcodes(*)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``command``: Name of program to be spawned (string, significant only at *root*).
* ``argv``: Arguments to *command* (array of strings, significant only at *root*).
* ``maxprocs``: Maximum number of processes to start (integer, significant only at *root*).
* ``info``: A set of key-value pairs telling the runtime system where and how to start the processes (handle, significant only at *root*).
* ``root``: Rank of process in which previous arguments are examined (integer).
* ``comm``: Intracommunicator containing group of spawning processes (handle).

OUTPUT PARAMETER
----------------
* ``intercomm``: Intercommunicator between original group and the newly spawned group (handle).
* ``array_of_errcodes``: One code per process (array of integers).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_spawn` tries to start *maxprocs* identical copies of the MPI
program specified by *command*, establishing communication with them and
returning an intercommunicator. The spawned processes are referred to as
children. The children have their own MPI_COMM_WORLD, which is separate
from that of the parents. :ref:`MPI_Comm_spawn` is collective over *comm*, and
also may not return until :ref:`MPI_Init` has been called in the children.
Similarly, :ref:`MPI_Init` in the children may not return until all parents
have called :ref:`MPI_Comm_spawn`. In this sense, :ref:`MPI_Comm_spawn` in the parents
and :ref:`MPI_Init` in the children form a collective operation over the union
of parent and child processes. The intercommunicator returned by
:ref:`MPI_Comm_spawn` contains the parent processes in the local group and the
child processes in the remote group. The ordering of processes in the
local and remote groups is the same as the as the ordering of the group
of the *comm* in the parents and of MPI_COMM_WORLD of the children,
respectively. This intercommunicator can be obtained in the children
through the function :ref:`MPI_Comm_get_parent`.

The MPI standard allows an implementation to use the MPI_UNIVERSE_SIZE
attribute of MPI_COMM_WORLD to specify the number of processes that will
be active in a program. Although this implementation of the MPI standard
defines MPI_UNIVERSE_SIZE, it does not allow the user to set its value.
If you try to set the value of MPI_UNIVERSE_SIZE, you will get an error
message.

The *command* Argument

The *command* argument is a string containing the name of a program to
be spawned. The string is null-terminated in C. In Fortran, leading and
trailing spaces are stripped. MPI looks for the file first in the
working directory of the spawning process.

The *argv* Argument

*argv* is an array of strings containing arguments that are passed to
the program. The first element of *argv* is the first argument passed to
*command*, not, as is conventional in some contexts, the command itself.
The argument list is terminated by NULL in C and an empty string in
Fortran (note that it is the MPI application's responsibility to ensure
that the last entry of the *argv* array is an empty string; the compiler
will not automatically insert it). In Fortran, leading and trailing
spaces are always stripped, so that a string consisting of all spaces is
considered an empty string. The constant MPI_ARGV_NULL may be used in C
and Fortran to indicate an empty argument list. In C, this constant is
the same as NULL.

In C, the :ref:`MPI_Comm_spawn` argument *argv* differs from the *argv*
argument of *main* in two respects. First, it is shifted by one element.
Specifically, *argv*\ [0] of *main* contains the name of the program
(given by *command*). *argv*\ [1] of *main* corresponds to *argv*\ [0]
in :ref:`MPI_Comm_spawn`, *argv*\ [2] of *main* to *argv*\ [1] of
:ref:`MPI_Comm_spawn`, and so on. Second, *argv* of :ref:`MPI_Comm_spawn` must be
null-terminated, so that its length can be determined. Passing an *argv*
of MPI_ARGV_NULL to :ref:`MPI_Comm_spawn` results in *main* receiving *argc* of
1 and an *argv* whose element 0 is the name of the program.

The *maxprocs* Argument

Open MPI tries to spawn *maxprocs* processes. If it is unable to spawn
*maxprocs* processes, it raises an error of class MPI_ERR_SPAWN. If MPI
is able to spawn the specified number of processes, :ref:`MPI_Comm_spawn`
returns successfully and the number of spawned processes, *m*, is given
by the size of the remote group of *intercomm*.

A spawn call with the default behavior is called hard. A spawn call for
which fewer than *maxprocs* processes may be returned is called soft.

The *info* Argument

The *info* argument is an opaque handle of type MPI_Info in C and
INTEGER in Fortran. It is a container for a number of user-specified
(*key,value*) pairs. *key* and *value* are strings (null-terminated
``char *`` in C, ``character*(*)`` in Fortran). Routines to create and
manipulate the *info* argument are described in Section 4.10 of the
MPI-2 standard.

For the SPAWN calls, *info* provides additional,
implementation-dependent instructions to MPI and the runtime system on
how to start processes. An application may pass MPI_INFO_NULL in C or
Fortran. Portable programs not requiring detailed control over process
locations should use MPI_INFO_NULL.

The following keys for *info* are recognized in Open MPI. (The reserved
values mentioned in Section 5.3.4 of the MPI-2 standard are not
implemented.)

::

   Key                    Type     Description
   ---                    ----     -----------

   host                   char *   Host on which the process should be
                                   spawned.  See the orte_host man
                                   page for an explanation of how this
                                   will be used.
   hostfile               char *   Hostfile containing the hosts on which
                                   the processes are to be spawned. See
                                   the orte_hostfile man page for
                                   an explanation of how this will be
                                   used.
   add-host               char *   Add the specified host to the list of
                                   hosts known to this job and use it for
                                   the associated process. This will be
                                   used similarly to the -host option.
   add-hostfile           char *   Hostfile containing hosts to be added
                                   to the list of hosts known to this job
                                   and use it for the associated
                                   process. This will be used similarly
                                   to the -hostfile option.
   wdir                   char *   Directory where the executable is
                                   located. If files are to be
                                   pre-positioned, then this location is
                                   the desired working directory at time
                                   of execution - if not specified, then
                                   it will automatically be set to
                                   ompi_preload_files_dest_dir.
   ompi_prefix            char *   Same as the --prefix command line
                                   argument to mpirun.
   ompi_preload_binary    bool     If set to true, pre-position the
                                   specified executable onto the remote
                                   host. A destination directory must
                                   also be provided.
   ompi_preload_files     char *   A comma-separated list of files that
                                   are to be pre-positioned in addition
                                   to the executable.  Note that this
                                   option does not depend upon
                                   ompi_preload_binary - files can
                                   be moved to the target even if an
                                   executable is not moved.
   ompi_stdin_target      char *   Comma-delimited list of ranks to
                                   receive stdin when forwarded.
   ompi_non_mpi           bool     If set to true, launching a non-MPI
                                   application; the returned communicator
                                   will be MPI_COMM_NULL. Failure to set
                                   this flag when launching a non-MPI
                                   application will cause both the child
                                   and parent jobs to "hang".
   ompi_param             char *   Pass an OMPI MCA parameter to the
                                   child job.  If that parameter already
                                   exists in the environment, the value
                                   will be overwritten by the provided
                                   value.
   mapper                 char *   Mapper to be used for this job
   map_by                 char *   Mapping directive indicating how
                                   processes are to be mapped (slot,
                                   node, socket, etc.).
   rank_by                char *   Ranking directive indicating how
                                   processes are to be ranked (slot,
                                   node, socket, etc.).
   bind_to                char *   Binding directive indicating how
                                   processes are to be bound (core, slot,
                                   node, socket, etc.).
   path                   char *   List of directories to search for
                                   the executable
   npernode               char *   Number of processes to spawn on
                                   each node of the allocation
   pernode                bool     Equivalent to npernode of 1
   ppr                    char *   Spawn specified number of processes
                                   on each of the identified object type
   env                    char *   Newline-delimited list of envars to
                                   be passed to the spawned procs

*bool* info keys are actually strings but are evaluated as follows: if
the string value is a number, it is converted to an integer and cast to
a boolean (meaning that zero integers are false and non-zero values are
true). If the string value is (case-insensitive) "yes" or "true", the
boolean is true. If the string value is (case-insensitive) "no" or
"false", the boolean is false. All other string values are unrecognized,
and therefore false.

The *root* Argument

All arguments before the *root* argument are examined only on the
process whose rank in *comm* is equal to *root*. The value of these
arguments on other processes is ignored.

The *array_of_errcodes* Argument

The *array_of_errcodes* is an array of length *maxprocs* in which MPI
reports the status of the processes that MPI was requested to start. If
all *maxprocs* processes were spawned, *array_of_errcodes* is filled in
with the value MPI_SUCCESS. If anyof the processes are *not* spawned,
*array_of_errcodes* is filled in with the value MPI_ERR_SPAWN. In C or
Fortran, an application may pass MPI_ERRCODES_IGNORE if it is not
interested in the error codes.


NOTES
-----

Completion of :ref:`MPI_Comm_spawn` in the parent does not necessarily mean
that :ref:`MPI_Init` has been called in the children (although the returned
intercommunicator can be used immediately).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_spawn_multiple`
   * :ref:`MPI_Comm_get_parent`
   * :ref:`mpirun(1) <man1-mpirun>`

.. _mpi_info_env:


MPI_Info_env
============

.. include_body

:ref:`MPI_INFO_ENV` |mdash| Static MPI_Info object containing info about the
application


DESCRIPTION
-----------

The MPI-3 standard established a static MPI_Info object named
:ref:`MPI_INFO_ENV` that can be used to access information about how the
application was executed from the run-time.


SUPPORTED FIELDS
----------------

command
   If available, the value will be set to argv[0]. Note that the value
   may not always be available - e.g., it is valid for a program to call
   :ref:`MPI_Init` with NULL parameters, in which case argv[0] will not be set
   if run as a singleton. This value will never be set in a Fortran
   program as the argv are not available.

argv
   The argv given for the application. If no arguments are passed to the
   application, then this value will not be set. It will also not be set
   in the case of a singleton that calls :ref:`MPI_Init` with NULL parameters,
   or a Fortran program.

maxprocs
   The number of processes in the job.

soft
   Open MPI does not support the *soft* option for specifying the number
   of processes to be executed, so this value is set to the same as
   *maxprocs*.

host
   The name of the host this process is executing upon - the value
   returned from *gethostname()*.

arch
   The architecture of the host this process is executing upon. This
   value indicates the underlying chip architecture (e.g., x86_64), if
   it can be determined.

wdir
   The working directory at the time of process launch by mpiexec. Note
   that this value will not be set for processes launched as singletons
   as there is no reliable way for the MPI library to determine the
   location.

file
   Although specified by the MPI-3 standard, no value is currently set
   for this field.

thread_level
   The requested MPI thread level - note that this may differ from the
   *actual* MPI thread level of the application.

ompi_num_apps
   The number of application contexts in an MPMD job. This is an Open
   MPI-specific field and value.

ompi_np
   The number of processes in each application context, provided as a
   space-delimited list of integers. This is an Open MPI-specific field
   and value.

ompi_first_rank
   The MPI rank of the first process in each application context,
   provided as a space-delimited list of integers This is an Open
   MPI-specific field and value.

ompi_positioned_file_dir
   If Open MPI was asked to pre-position files, this field provides the
   top-level directory where those files were place. This is an Open
   MPI-specific field and value.


ERRORS
------

| When calling :ref:`MPI_INFO_GET`, the *flag* parameter will be set to zero
  (false) if a value for the field has not been set.


.. seealso::
   * :ref:`MPI_Info_get`

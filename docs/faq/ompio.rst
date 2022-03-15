Open MPI IO ("OMPIO")
=====================

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

What is the OMPIO?
------------------

OMPIO is an implementation of the MPI I/O functions defined in version
two of the Message Passing Interface specification.  The main goals of
OMPIO are:

#. Increase the modularity of the parallel I/O library by separating
   MPI I/O functionality into sub-frameworks.

#. Allow frameworks to utilize different run-time decision algorithms
   to determine which module to use in a particular scenario, enabling
   non-file-system-specific decisions.

#. Improve the integration of parallel I/O functions with other
   components of Open MPI, most notably the derived data type engine
   and the progress engine. The integration with the derived data type
   engine allows for faster decoding of derived data types and the
   usage of optimized data type to data type copy operations.

OMPIO is fundamentally a component of the ``io`` framework in Open
MPI. Upon opening a file, the OMPIO component initializes a number of
sub-frameworks and their components, namely:

*  ``fs``: responsible for all file management operations
* ``fbtl``: support for individual blocking and non-blocking
  I/O operations
* ``fcoll``: support for collective blocking and non-blocking I/O
  operations
* ``sharedfp``: support for all shared file pointer operations.

/////////////////////////////////////////////////////////////////////////

How can I use OMPIO?
--------------------

OMPIO is included in Open MPI and is used by default when invoking
MPI IO API functions.

/////////////////////////////////////////////////////////////////////////

How do I know what MCA parameters are available for tuning the performance of OMPIO?
------------------------------------------------------------------------------------

The ``ompi_info`` command can display all the parameters available for the
OMPIO ``io``, ``fcoll``, ``fs``, and ``sharedfp`` components:

.. code-block:: sh

   shell$ ompi_info --param io       ompio
   shell$ ompi_info --param fcoll    all
   shell$ ompi_info --param fs       all
   shell$ ompi_info --param sharedfp all

/////////////////////////////////////////////////////////////////////////

How can I choose the right component for a sub-framework of OMPIO?
------------------------------------------------------------------

The OMPIO architecture is designed around sub-frameworks, which allow
you to develop a relatively small amount of code optimized for a
particular environment, application, or infrastructure.  Although
significant efforts have been invested into making good decisions for
default values and switching points between components, users and/or
system administrators might occasionally want to tune the selection
logic of the components and force the utilization of a particular
component.

The simplest way to force the usage of a component is to simply
restrict the list of available components for that framework. For
example, an application wanting to use the ``dynamic`` ``fcoll``
component simply has to pass the name of the component as a value to
the corresponding MCA parameter during ``mpirun`` or any other
mechanism available in Open MPI to influence a parameter value, e.g.:

.. code-block:: sh

   shell$ mpirun --mca fcoll dynamic -n 64 ./a.out

``fs`` and ``fbtl`` components are typically chosen based on the file
system type utilized (e.g. the ``pvfs2`` component is chosen when the
file is located on an PVFS2 file system, the ``lustre`` component is
chosen for Lustre file systems, etc.).

The ``fcoll`` framework provides several different implementations,
which provide different levels of data reorganization across
processes.  ``two_phase``, ``dynamic`` segmentation, ``static``
segmentation and ``individual`` provide decreasing communication costs
during the shuffle phase of the collective I/O operations (in the
order listed here), but provide also decreasing contiguity guarantuees
of the data items before the aggregators read/write data to/from the
file.  The current decision logic in OMPIO is using the file view
provided by the application as well as file system level
characteristics (stripe width of the file system) in the selection
logic of the fcoll framework.

The ``sharedfp`` framework provides a different implementation of the
shared file pointer operations depending on file system features, such
as:

* ``lockfile``: support for file locking.
* ``sm``: locality of the MPI processes in the communicator that has
  been used to open the file.
* ``individual``: guarantees by the application on using only a subset
  of the available functionality (i.e. write operations only).

/////////////////////////////////////////////////////////////////////////

How can I tune OMPIO parameters to improve performance?
-------------------------------------------------------

The most important parameters influencing the performance of an I/O
operation are listed below:

#. ``io_ompio_cycle_buffer_size``: Data size issued by individual
   reads/writes per call. By default, an individual read/write
   operation will be executed as one chunk. Splitting the operation up
   into multiple, smaller chunks can lead to performance improvements
   in certain scenarios.

#. ``io_ompio_bytes_per_agg``: Size of temporary buffer for collective
   I/O operations on aggregator processes. Default value is 32MB.
   Tuning this parameter has a very high impact on the performance of
   collective operations.

   .. note:: Be sure to also see recommendations for tuning collective
             operations.

#. ``io_ompio_num_aggregators``: Number of aggregators used in
   collective I/O operations.  Setting this parameter to a value
   larger zero disables the internal automatic aggregator selection
   logic of OMPIO.  Tuning this parameter has a very high impact on
   the performance of collective operations.

   .. note:: Be sure to also see recommendations for tuning collective
             operations.

#. ``io_ompio_grouping_option``: Algorithm used to automatically
   decide the number of aggregators used. Applications working with
   regular 2-D or 3-D data decomposition can try changing this
   parameter to 4 (hybrid) algorithm.

/////////////////////////////////////////////////////////////////////////

What are the main parameters of the ``fs`` framework and components?
--------------------------------------------------------------------

The main parameters of the ``fs`` components allow you to manipulate
the layout of a new file on a parallel file system.

#. ``fs_pvfs2_stripe_size``: Sets the number of storage servers for a
   new file on a PVFS2 file system. If not set, system default will be
   used. Note that this parameter can also be set through the
   ``stripe_size`` MPI Info value.

#. ``fs_pvfs2_stripe_width``: Sets the size of an individual block for
   a new file on a PVFS2 file system. If not set, system default will
   be used. Note that this parameter can also be set through the
   ``stripe_width`` MPI Info value.

#. ``fs_lustre_stripe_size``: Sets the number of storage servers for a
   new file on a Lustre file system. If not set, system default will
   be used. Note that this parameter can also be set through the
   ``stripe_size`` MPI Info value.

#. ``fs_lustre_stripe_width``: Sets the size of an individual block
   for a new file on a Lustre file system. If not set, system default
   will be used. Note that this parameter can also be set through the
   ``stripe_width`` MPI Info value.

////////////////////////////////////////////////////////////////////////

What are the main parameters of the ``fbtl`` framework and components?
----------------------------------------------------------------------

No performance relevant parameters are currently available for the
``fbtl`` components.

/////////////////////////////////////////////////////////////////////////

What are the main parameters of the ``fcoll`` framework and components?
-----------------------------------------------------------------------

The design of the ``fcoll`` frameworks maximizes the utilization of
parameters of the OMPIO component, in order to minimize the number of similar
MCA parameters in each component.

For example, the ``two_phase``, ``dynamic``, and ``static`` components
all retrieve the ``io_ompio_bytes_per_agg`` parameter to define the
collective buffer size and the ``io_ompio_num_aggregators`` parameter
to force the utilization of a given number of aggregators.

/////////////////////////////////////////////////////////////////////////

What are the main parameters of the ``sharedfp`` framework and components?
--------------------------------------------------------------------------

No performance relevant parameters are currently available for the
``sharedfp`` components.

/////////////////////////////////////////////////////////////////////////

How do I tune collective I/O operations?
----------------------------------------

The most influential parameter that can be tuned in advance is the
``io_ompio_bytes_per_agg`` parameter of the ``ompio`` component. This
parameter is essential for the selection of the collective I/O
component as well for determining the optimal number of aggregators
for a collective I/O operation. It is a file system-specific value,
independent of the application scenario. To determine the correct
value on your system, take an I/O benchmark (e.g., the IMB or IOR
benchmark) and run an individual, single process write test. E.g., for
IMB:

.. code-block:: sh

   shell$ mpirun -n 1 ./IMB-IO S_write_indv

For IMB, use the values obtained for AGGREGATE test cases. Plot the
bandwidth over the message length. The recommended value for
``io_ompio_bytes_per_agg`` is the smallest message length which
achieves (close to) maximum bandwidth from that process's
perspective.

.. note:: Make sure that the ``io_ompio_cycle_buffer_size`` parameter
          is set to -1 when running this test, which is its default
          value).  The value of ``io_ompio_bytes_per_agg`` could be
          set by system administrators in the system-wide Open MPI
          configuration file, or by users individually. See :ref:`this
          FAQ item <faq-tuning-setting-mca-params-label>` on setting
          MCA parameters for details.

For more exhaustive tuning of I/O parameters, we recommend the
utilization of the `Open Tool for Parameter Optimization (OTPO)
<https://www.open-mpi.org/projects/otpo>`_, a tool specifically
designed to explore the MCA parameter space of Open MPI.

/////////////////////////////////////////////////////////////////////////

When should I use the ``individual`` ``sharedfp`` component, and what are its limitations?
------------------------------------------------------------------------------------------

The ``individual`` sharedfp component provides an approximation of
shared file pointer operations that can be used for *write operations
only*. It is only recommended in scenarios, where neither the ``sm``
nor the ``lockedfile`` component can be used, e.g., due to the fact
that more than one node are being used and the file system does not
support locking.

Conceptually, each process writes the data of a write_shared operation
into a separate file along with a time stamp. In every collective
operation (latest in file_close), data from all individual files are
merged into the actual output file, using the time stamps as the main
criteria.

The component has certain limitations and restrictions, such as its
relience on the synchronization accuracy of the clock on the cluster
to determine the order between entries in the final file, which might
lead to some deviations compared to the actual calling sequence.

/////////////////////////////////////////////////////////////////////////

What other features of OMPIO are available?
-------------------------------------------

OMPIO has a number of additional features, mostly directed towards
developers, which could occasionally also be useful to interested
end-users. These can typically be controlled through MCA parameters.

* ``io_ompio_sharedfp_lazy_open``: By default, ``ompio`` does not
  establish the necessary data structures required for shared file
  pointer operations during file_open. It delays generating these data
  structures until the first utilization of a shared file pointer
  routine. This is done mostly to minimize the memory footprint of
  ``ompio``, and due to the fact that shared file pointer operations
  are rarely used compared to the other functions. Setting this
  parameter to 0 disables this optimization.

* ``io_ompio_coll_timing_info``: Setting this parameter will lead to a
  short report upon closing a file indicating the amount of time spent
  in communication and I/O operations of collective I/O operations
  only.

* ``io_ompio_record_file_offset_info``: Setting this parameter will
  report neighborhood relationship of processes based on the file view
  used. This is occasionally important for understanding performance
  characteristics of I/O operations.  Note, that using this features
  requires an additional compile time flag when compiling ``ompio``.

  The output file generated as a result of this flag provides the
  access pattern of processes to the file recorded as neighborhood
  relationships of processes as a matrix. For example, if the first
  four bytes of a file are being accessed by process 0 and the next
  four bytes by process 1, processes 0 and 1 are said to have a
  neighborhood relationship since they access neighboring elements of
  the file.  For each neighborhood relation detected in the file, the
  value for the corresponding pair of processes is increased by one.

  Data is provided in compressed row storage format. To minimize the
  amount of data written using this feature, only non-zero values are
  output.  The first row in the output file indicates the number of
  non-zero elements in the matrix; the second number is the number of
  elements in the row index.  The third row of the output file gives
  all the column indexes. The fourth row lists all the values and the
  fifth row gives the row index. A row index represents the position
  in the value array where a new row starts.

/////////////////////////////////////////////////////////////////////////

Known limitations
-----------------

OMPIO implements most of the I/O functionality of the MPI
specification. There are, however, two not very commonly used
functions that are not implemented as of today:

* Switching from the relaxed consistency semantics of MPI to stricter, sequential
  consistency through the MPI_File_set_atomicity functions

* Using user defined data representations

.. error:: TODO Are these still accurate?

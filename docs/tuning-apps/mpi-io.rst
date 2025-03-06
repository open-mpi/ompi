Open MPI IO ("OMPIO")
=====================

OMPIO is an Open MPI-native implementation of the MPI I/O functions
defined in the MPI specification.

The main goals of OMPIO are:

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
MPI.  Upon opening a file, the OMPIO component initializes a number of
sub-frameworks and their components, namely:

*  ``fs``: responsible for all file management operations
* ``fbtl``: support for blocking and non-blocking individual
  I/O operations
* ``fcoll``: support for blocking and non-blocking collective I/O
  operations
* ``sharedfp``: support for all shared file pointer operations.


MCA parameters of OMPIO and associated frameworks
-------------------------------------------------

The :ref:`ompi_info(1) <man1-ompi_info>` command can display all the
parameters available for the OMPIO ``io``, ``fcoll``, ``fs``,
``fbtl``, and ``sharedfp`` components:

.. code-block:: sh

   shell$ ompi_info --param io       ompio --level 9
   shell$ ompi_info --param fcoll    all --level 9
   shell$ ompi_info --param fs       all --level 9
   shell$ ompi_info --param fbtl     all --level 9
   shell$ ompi_info --param sharedfp all --level 9

OMPIO sub-framework components
------------------------------

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
file is located on an PVFS2/OrangeFS file system, the ``lustre``
component is chosen for Lustre file systems, etc.). The ``ufs`` ``fs``
component is used if no file system specific component is availabe
(e.g. local file systems, NFS, BeefFS, etc.), and the ``posix``
``fbtl`` component is used as the default component for read/write
operations.

The ``fcoll`` framework provides several different components. The
current decision logic in OMPIO uses the file view provided by the
application as well as file system level characteristics (e.g. file
system, stripe width) to determine which component to use. The most
important ``fcoll`` components are:

* ``dynamic_gen2``: the default component used on lustre file
  system. This component is based on the two-phase I/O algorithm with
  a static file partioning strategy, i.e. an aggregator processes will
  by default only write data to a single storage server.

* ``vulcan``: the default component used on all other file
  systems. This component is based on the two-phase I/O algorithm with
  an even file partitioning strategy, i.e. each of the n aggregators
  will write 1/n th of the overall file.

* ``individual``: this components executes all collective I/O
  operations in terms of individual I/O operations.

The ``sharedfp`` framework provides a different implementation of the
shared file pointer operations depending on file system features.

* ``lockedfile``: this component will be used on file systems which
  support file locking.

* ``sm``: component used in scenarios in which all processes of the
  communicator are on the same physical node.

* ``individual``: a component that can be used if neither of the other
  two components are available. This component provides however only
  limited functionality (i.e. write operations only).

  .. note:: See :ref:`the section on the individual sharedfp component
            <label-ompio-individual-sharedfp>` to understand
            functionality and limitations.

Tuning OMPIO performance
------------------------

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

#. ``io_ompio_num_aggregators``: Number of aggregators used in
   collective I/O operations.  Setting this parameter to a value
   larger zero disables the internal automatic aggregator selection
   logic of OMPIO.  Tuning this parameter has a very high impact on
   the performance of collective operations.

#. ``io_ompio_grouping_option``: Algorithm used to automatically
   decide the number of aggregators used. Applications working with
   regular 2-D or 3-D data decomposition can try changing this
   parameter to 4 (hybrid) algorithm.

#. ``fs_ufs_lock_algorithm``: Parameter used to determing what part of
   a file needs to be locked for a file operation. Since the ``ufs``
   ``fs`` component is used on multiple file systems, OMPIO
   automatically chooses the value required for correctness on all
   file systems, e.g. enforcing locking on an NFS file system, while
   disabling locking on a local file system. Users can adjust the
   required locking behavior based on their use case, since the
   default value might often be too restrictive for their application.

Setting stripe size and stripe width on parallel file systems
-------------------------------------------------------------

Many ``fs`` components allow you to manipulate the layout of a new
file on a parallel file system.  Note, that many file systems only
allow changing these setting upon file creation, i.e. modifying these
values for an already existing file might not be possible.

#. ``fs_pvfs2_stripe_size``: Sets the number of storage servers for a
   new file on a PVFS2/OrangeFS  file system. If not set, system default will be
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

Using GPU device buffers in MPI File I/O operations
----------------------------------------------------

OMPIO supports reading and writing directly to/from GPU buffers using
the MPI I/O interfaces. Using this feature simplifies managing buffers
that are exclusively used on GPU devices, and hence there is no need to
implement staging through host memory for file I/O operations.

Internally, OMPIO splits a user buffer into chunks for performing the
read/write operation. The chunk-size used by OMPIO can have a
significant influence on the performance of the file I/O operation
from device buffers, and can be controlled using the
``io_ompio_pipeline_buffer_size`` MCA parameter.

.. _label-ompio-individual-sharedfp:

Using the ``individual`` ``sharedfp`` component and its limitations
-------------------------------------------------------------------

The ``individual`` sharedfp component provides an approximation of
shared file pointer operations that can be used for *write operations
only*. It is only recommended in scenarios, where neither the ``sm``
nor the ``lockedfile`` component can be used, e.g., due to the fact
that more than one node are being used and the file system does not
support locking.

Conceptually, each process writes the data of a write_shared operation
into a separate file along with a time stamp. In every collective
operation (or during the file_close operation), data from all
individual files are merged into the actual output file, using the
time stamps as the main criteria.

The component has certain limitations and restrictions, such as its
relience on the synchronization clocks on the individual cluster nodes
to determine the order between entries in the final file, which might
lead to some deviations compared to the actual calling sequence.

Furthermore, the component only supports ``write`` operations, read
operations are not supported.

Other features of OMPIO
-----------------------

OMPIO has a number of additional features, mostly directed towards
developers, which could occasionally also be useful to interested
end-users. These can typically be controlled through MCA parameters.

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

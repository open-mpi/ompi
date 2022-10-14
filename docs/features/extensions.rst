.. _ompi-features-extentions-label:

Open MPI extensions
===================

Open MPI contains a framework for extending the MPI API that is
available to applications.  Each extension is usually a standalone set
of functionality that is distinct from other extensions (similar to
how Open MPI's plugins are usually unrelated to each other).  These
extensions provide new functions and/or constants that are available
to MPI applications.

.. warning:: These extensions are neither standard nor portable to
   other MPI implementations!

   They are a mechanism for the Open MPI developer community to
   provide new functionality to users, typically before it becomes
   standardized by the MPI Forum.

Available extensions
--------------------

The following extensions are included in this version of Open MPI:

#. ``shortfloat``: Provides MPI datatypes ``MPIX_C_FLOAT16``,
   ``MPIX_SHORT_FLOAT``, ``MPIX_SHORT_FLOAT``, and
   ``MPIX_CXX_SHORT_FLOAT_COMPLEX`` if corresponding language types are
   available. See ``ompi/mpiext/shortfloat/README.txt`` for details.
#. ``affinity``: Provides the ``OMPI_Affinity_str()`` API, which returns
   a string indicating the resources which a process is bound. For
   more details, see its man page.
#. ``cuda``: When the library is compiled with CUDA-aware support, it
   provides two things.  First, a macro
   ``MPIX_CUDA_AWARE_SUPPORT``. Secondly, the function
   ``MPIX_Query_cuda_support()`` that can be used to query for support.
#. ``example``: A non-functional extension; its only purpose is to
   provide an example for how to create other extensions.
#. ``ftmpi``: An implementation of the User Level Fault Mitigation
   (ULFM) proposal.  :ref:`See its documentation section <ulfm-label>`
   for more details.

Compiling the extensions
------------------------

Open MPI extensions are all enabled by default; they can be disabled
via the ``--disable-mpi-ext`` command line switch.

Since extensions are meant to be used by advanced users only, this
file does not document which extensions are available or what they do.
Look in the ``ompi/mpiext`` directory in a distribution Open MPI
tarball to see the extensions; each subdirectory of that directory
contains an extension.  Each has a ``README`` file that describes what
it does.

Using the extensions
--------------------

To reinforce the fact that these extensions are non-standard, you must
include a separate header file after ``<mpi.h>`` to obtain the function
prototypes, constant declarations, etc.  For example:

.. code-block:: c

   #include <mpi.h>
   #if defined(OPEN_MPI) && OPEN_MPI
   #include <mpi-ext.h>
   #endif

   int main() {
       MPI_Init(NULL, NULL);

   #if defined(OPEN_MPI) && OPEN_MPI
       char ompi_bound[OMPI_AFFINITY_STRING_MAX];
       char current_binding[OMPI_AFFINITY_STRING_MAX];
       char exists[OMPI_AFFINITY_STRING_MAX];

       OMPI_Affinity_str(OMPI_AFFINITY_LAYOUT_FMT, ompi_bound,
                         current_bindings, exists);
   #endif

       MPI_Finalize();
       return 0;
   }

Notice that the Open MPI-specific code is surrounded by the ``#if``
statement to ensure that it is only ever compiled by Open MPI.

The Open MPI wrapper compilers (``mpicc`` and friends) should
automatically insert all relevant compiler and linker flags necessary
to use the extensions.  No special flags or steps should be necessary
compared to "normal" MPI applications.

.. _ompi-features-extensions-label:

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

The following extensions are included in this version of Open MPI.
Follow the link on each name for a full description of what it
provides, when it is built, when it is available at run time, and the
functions it offers:

* :doc:`affinity </features/extension-affinity>`: Provides the
  ``OMPI_Affinity_str()`` API, which returns human-readable strings
  describing how the calling process is bound to processor resources.

* :doc:`cuda </tuning-apps/accelerators/cuda>`: Provides the
  ``MPIX_CUDA_AWARE_SUPPORT`` compile-time macro and the
  ``MPIX_Query_cuda_support()`` run-time function for detecting whether
  the library has NVIDIA CUDA-aware support.

* :doc:`rocm </tuning-apps/accelerators/rocm>`: Provides the
  ``MPIX_ROCM_AWARE_SUPPORT`` compile-time macro and the
  ``MPIX_Query_rocm_support()`` run-time function for detecting whether
  the library has AMD ROCm-aware support.

* :doc:`ftmpi </features/ulfm>`: An implementation of the MPI Forum's
  User-Level Failure Mitigation (ULFM) proposal, providing the
  ``MPIX_Comm_*`` functions and ``MPIX_ERR_*`` error codes for writing
  fault-tolerant MPI applications.

* :doc:`shortfloat </features/extension-shortfloat>`: Provides MPI
  datatypes corresponding to short / half-precision floating point C
  and C++ language types, when such types are available.

* :doc:`example </features/extension-example>`: A non-functional
  extension whose only purpose is to demonstrate how to create a new
  Open MPI extension.

.. toctree::
   :hidden:

   extension-affinity
   extension-shortfloat
   extension-example

Compiling the extensions
------------------------

Most Open MPI extensions are enabled by default; the exceptions are
extensions that require functionality not present in your build
environment (for example, ``shortfloat`` is only built when the
compiler provides a suitable short / half-precision floating point
type) and the developer-only ``example`` extension (which is only
built when explicitly requested).

The set of extensions to build is selected at configure time:

* ``--enable-mpi-ext`` (the default) builds all available extensions.
* ``--enable-mpi-ext=LIST`` builds only the comma-separated extensions
  named in ``LIST`` |mdash| for example,
  ``--enable-mpi-ext=cuda,rocm``.
* ``--disable-mpi-ext`` builds none of the extensions.

Each extension's own page (linked above) documents any additional
build-time prerequisites and the configure options needed to satisfy
them.

You can confirm which extensions were compiled into a given Open MPI
installation with ``ompi_info``:

.. code-block:: sh

   shell$ ompi_info | grep "MPI extensions"
          MPI extensions: affinity, cuda, ftmpi, rocm

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
                         current_binding, exists);
   #endif

       MPI_Finalize();
       return 0;
   }

Notice that the Open MPI-specific code is surrounded by the ``#if``
statement to ensure that it is only ever compiled by Open MPI.

Including ``<mpi-ext.h>`` defines the preprocessor macro
``OMPI_HAVE_MPI_EXT`` to ``1``.  In addition, for each extension that
is present, it defines a macro named ``OMPI_HAVE_MPI_EXT_<NAME>`` (with
``<NAME>`` being the uppercased extension name, e.g.,
``OMPI_HAVE_MPI_EXT_AFFINITY``) to ``1``.  Applications can test these
macros to portably guard their use of a given extension |mdash| both
against Open MPI builds that omitted the extension and against other
MPI implementations that do not provide ``<mpi-ext.h>`` at all.

The Open MPI wrapper compilers (``mpicc`` and friends) should
automatically insert all relevant compiler and linker flags necessary
to use the extensions.  No special flags or steps should be necessary
compared to "normal" MPI applications.

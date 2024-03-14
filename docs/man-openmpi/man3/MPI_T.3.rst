.. _mpi_t:

MPI_T
=====

.. include_body

Open MPI's MPI_T interface - General information

DESCRIPTION
-----------

There are a few Open MPI-specific notes worth mentioning about its MPI_T
interface implementation.

MPI_T Control Variables
^^^^^^^^^^^^^^^^^^^^^^^

Open MPI's implementation of the MPI_T Control Variable ("cvar") APIs is
an interface to Open MPI's underlying Modular Component Architecture
(MCA) parameters/variables. Simply put: using the MPI_T cvar interface
is another mechanism to get/set Open MPI MCA parameters.

In order of precedence (highest to lowest), Open MPI provides the
following mechanisms to set MCA parameters:

1. The MPI_T interface has the highest precedence. Specifically: values
   set via the MPI_T interface will override all other settings.
2. The mpirun(1) / mpiexec(1) command line (e.g., via the --mca
   parameter).
3. Environment variables.
4. Parameter files have the lowest precedence. Specifically: values set
   via parameter files can be overridden by any of the other
   MCA-variable setting mechanisms.

MPI initialization
^^^^^^^^^^^^^^^^^^

An application may use the MPI_T interface before MPI is initialized to
set MCA parameters. Setting MPI-level MCA parameters before MPI is
initialized may affect *how* MPI is initialized (e.g., by influencing
which frameworks and components are selected).

The following example sets the pml and btl MCA params before invoking
:ref:`MPI_Init` in order to force a specific selection of PML and BTL
components:

.. code:: c

   int provided, index, count;
   MPI_T_cvar_handle pml_handle, btl_handle;
   char pml_value[64], btl_value[64];

   MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);

   MPI_T_cvar_get_index("pml", &index);
   MPI_T_cvar_handle_alloc(index, NULL, &pml_handle, &count);
   MPI_T_cvar_write(pml_handle, "ob1");

   MPI_T_cvar_get_index("btl", &index);
   MPI_T_cvar_handle_alloc(index, NULL, &btl_handle, &count);
   MPI_T_cvar_write(btl_handle, "tcp,vader,self");

   MPI_T_cvar_read(pml_handle, pml_value);
   MPI_T_cvar_read(btl_handle, btl_value);
   printf("Set value of cvars: PML: %s, BTL: %s\n",
          pml_value, btl_value);

   MPI_T_cvar_handle_free(&pml_handle);
   MPI_T_cvar_handle_free(&btl_handle);

   MPI_Init(NULL, NULL);

   // ...

   MPI_Finalize();

   MPI_T_finalize();

Note that once MPI is initialized, most Open MPI cvars become read-only.

For example, after MPI is initialized, it is no longer possible to set
the PML and BTL selection mechanisms. This is because many of these MCA
parameters are only used during MPI initialization; setting them after
MPI has already been initialized would be meaningless, anyway.

MPI_T Categories
^^^^^^^^^^^^^^^^

Open MPI's MPI_T categories are organized hierarchically:

1. Layer (or "project"). There are two layers in Open MPI:

   -  ompi: This layer contains cvars, pvars, and sub categories related
      to MPI characteristics.
   -  opal: This layer generally contains cvars, pvars, and sub
      categories of lower-layer constructions, such as operating system
      issues, networking issues, etc.

2. Framework or section.

   -  In most cases, the next level in the hierarchy is the Open MPI MCA
      framework.

      -  For example, you can find the btl framework under the opal
         layer (because it has to do with the underlying networking).
      -  Additionally, the pml framework is under the ompi layer
         (because it has to do with MPI semantics of point-to-point
         messaging).

   -  There are a few non-MCA-framework entities under the layer,
      however.

      -  For example, there is an mpi section under both the opal and
         ompi layers for general/core MPI constructs.

3. Component.

   -  If relevant, the third level in the hierarchy is the MCA
      component.
   -  For example, the tcp component can be found under the opal
      framework in the opal layer.


.. seealso::
   * :ref:`MPI_T_init_thread`

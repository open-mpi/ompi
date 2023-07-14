.. _label-removed-mpi-constructs:

Removed MPI constructs
======================

Starting with v4.0.0, Open MPI |mdash| by default |mdash| removes the
prototypes from ``mpi.h`` for MPI symbols that were deprecated in 1996
in the MPI-2.0 standard, and finally removed from the MPI-3.0 standard
(2012).

Specifically, the following symbols (specified in the MPI
language-neutral names) are no longer prototyped in ``mpi.h`` by
default:

.. note:: You may need to scroll right in the following table.

.. list-table::
    :header-rows: 1

    * - Removed symbol

        (click for more details, below)
      - Replaced with

        (click to go to the corresponding man page)
      - Deprecated
      - Removed

    * - :ref:`MPI_ADDRESS <label-mpi-address>`
      - :ref:`MPI_GET_ADDRESS <mpi_get_address>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_ERRHANDLER_CREATE <label-mpi-errhandler-create>`
      - :ref:`MPI_COMM_CREATE_ERRHANDLER <mpi_comm_create_errhandler>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_ERRHANDLER_GET <label-mpi-errhandler-get>`
      - :ref:`MPI_COMM_GET_ERRHANDLER <mpi_comm_get_errhandler>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_ERRHANDLER_SET <label-mpi-errhandler-set>`
      - :ref:`MPI_COMM_SET_ERRHANDLER <mpi_comm_set_errhandler>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_TYPE_EXTENT <label-mpi-type-extent>`
      - :ref:`MPI_TYPE_GET_EXTENT <mpi_type_get_extent>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_TYPE_HINDEXED <label-mpi-type-hindexed>`
      - :ref:`MPI_TYPE_CREATE_HINDEXED <mpi_type_create_hindexed>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_TYPE_HVECTOR <label-mpi-type-hvector>`
      - :ref:`MPI_TYPE_CREATE_HVECTOR <mpi_type_create_hvector>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_TYPE_LB <label-mpi-type-lb>`
      - :ref:`MPI_TYPE_GET_EXTENT <mpi_type_get_extent>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_TYPE_STRUCT <label-mpi-type-struct>`
      - :ref:`MPI_TYPE_CREATE_STRUCT <mpi_type_create_struct>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_TYPE_UB <label-mpi-type-ub>`
      - :ref:`MPI_TYPE_GET_EXTENT <mpi_type_get_extent>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_LB <label-mpi-lb-ub>`
      - :ref:`MPI_TYPE_CREATE_RESIZED <mpi_type_create_resized>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_UB <label-mpi-lb-ub>`
      - :ref:`MPI_TYPE_CREATE_RESIZED <mpi_type_create_resized>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_COMBINED_HINDEXED_INTEGER <label-mpi-combiner-fortran-integers>`
      - :ref:`MPI_COMBINER_HINDEXED <mpi_type_get_envelope>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_COMBINED_HVECTOR_INTEGER <label-mpi-combiner-fortran-integers>`
      - :ref:`MPI_COMBINER_HVECTOR <mpi_type_get_envelope>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_COMBINED_STRUCT_INTEGER <label-mpi-combiner-fortran-integers>`
      - :ref:`MPI_COMBINER_STRUCT <mpi_type_get_envelope>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

    * - :ref:`MPI_HANDLER_FUNCTION <label-mpi-handler-function>`
      - :ref:`MPI_COMM_ERRHANDLER_FUNCTION <mpi_comm_create_errhandler>`
      - MPI-2.0 (1996)
      - MPI-3.0 (2012)

Although these symbols are no longer prototyped in ``mpi.h``, *they are
still present in the MPI library in Open MPI* |ompi_series|. This enables
legacy MPI applications to *link and run* successfully with Open MPI
|ompi_series|, even though they will fail to *compile*.

Furthermore, the MPI C++ interfaces were deprecated in version
2.2, and then were removed in MPI-3.0.  Starting
from v5.0.0, Open MPI does not support the C++ interfaces
any more. Users who would like to continue using the C++ interfaces of
MPI will need to use an older release of Open MPI.

.. warning:: The Open MPI team **strongly** encourages all
   MPI application developers to stop using these constructs that were
   first deprecated over 20 years ago, and finally removed from the MPI
   specification in MPI-3.0 (in 2012).

The FAQ items in this category show how to update your application to
stop using these removed symbols.

All that being said, if you are unable to immediately update your
application to stop using these removed MPI-1 symbols, you can
re-enable them in ``mpi.h`` by configuring Open MPI with the
``--enable-mpi1-compatibility`` flag.

.. note:: Future releases of Open MPI may remove these symbols
   altogether.

Why is Open MPI breaking the compilation of MPI applications?
------------------------------------------------------------------

The Open MPI developer community decided to take a first step of
removing prototypes of deprecated functions from ``mpi.h`` starting
with the Open MPI v4.0.x series for the following reasons:

#. The first set of symbols have been deprecated since *1996.* It's
   time to start raising awareness for developers who are
   inadvertently still using these removed symbols.
#. The MPI Forum removed a substantial set of symbols from the MPI-3.0
   specification in 2012. This is a sign that the Forum itself
   recognizes that these removed symbols are no longer needed.
#. More functions have been deprecated in MPI 2.2 and MPI 4.0, and
   additional functions are expected to be deprecated and removed in
   future MPI versions. It is in the interest of both, developers and
   end-users, to minimize confusion as much as possible, and stick
   closely to the MPI specification.
#. Note that Open MPI *did not fully remove* these removed symbols: we
   just made it slightly more painful to get to them.  This is an
   attempt to raise awareness so that MPI application developers can
   update their applications (it's easy!).

In short: the only way to finally be able to remove these removed
symbols from Open MPI someday is to have a "grace period" where the
MPI application developers are a) made aware that they are using
removed symbols, and b) educated how to update their applications.

We, the Open MPI developers, recognize that your MPI application
failing to compile with Open MPI may be a nasty surprise.  We
apologize for that.

Our intent is simply to use this minor shock to raise awareness and
use it as an educational opportunity to show you how to update your
application (or direct your friendly neighborhood MPI application
developer to this FAQ) to stop using these removed MPI symbols.

Thank you!

.. _label-mpi-address:

Stop using MPI_ADDRESS
----------------------

In C, the only thing that changed was the function name:
``MPI_Address()`` |rarrow| ``MPI_Get_address()``.  Nothing else needs
to change:

.. code-block:: c++

    char buffer[30];
    MPI_Aint address;

    // Old way
    MPI_Address(buffer, &address);

    // New way
    MPI_Get_address(buffer, &address);

In Fortran, the type of the parameter changed from ``INTEGER``
|rarrow| ``INTEGER(KIND=MPI_ADDRESS_KIND)`` so that it can hold
larger values (e.g., 64 bit pointers):

.. code-block:: Fortran

    USE mpi
    REAL buffer
    INTEGER ierror
    INTEGER old_address
    INTEGER(KIND = MPI_ADDRESS_KIND) new_address

    ! Old way
    CALL MPI_ADDRESS(buffer, old_address, ierror)

    ! New way
    CALL MPI_GET_ADDRESS(buffer, new_address, ierror)

.. _label-mpi-errhandler-create:

Stop using MPI_ERRHANDLER_CREATE
--------------------------------

In C, *effectively* the only thing that changed was the name of the
function: ``MPI_Errhandler_create()`` |rarrow|
``MPI_Comm_create_errhandler()``.

*Technically*, the type of the first parameter also changed
(``MPI_Handler_function`` |rarrow| ``MPI_Comm_errhandler_function``),
but most applications do not use this type directly and may not even
notice the change.

.. code-block:: c++

    void my_errhandler_function(MPI_Comm *comm, int *code, ...)
    {
        // Do something useful to handle the error
    }

    void some_function(void)
    {
        MPI_Errhandler my_handler;

        // Old way
        MPI_Errhandler_create(my_errhandler_function, &my_handler);

        // New way
        MPI_Comm_create_errhandler(my_errhandler_function, &my_handler);
    }

In Fortran, only the subroutine name changed:
``MPI_ERRHANDLER_CREATE`` |rarrow| ``MPI_COMM_CREATE_ERRHANDLER``.

.. code-block:: Fortran

    USE mpi
    EXTERNAL my_errhandler_function
    INTEGER ierror
    INTEGER my_handler

    ! Old way
    CALL MPI_ERRHANDLER_CREATE(my_errhandler_function, my_handler, ierror)

    ! New way
    CALL MPI_COMM_CREATE_ERRHANDLER(my_errhandler_function, my_handler, ierror)

.. _label-mpi-errhandler-get:

Stop using MPI_ERRHANDLER_GET
-----------------------------

In both C and Fortran, the only thing that changed with regards to
``MPI_ERRHANDLER_GET`` is the name: ``MPI_ERRHANDLER_GET`` |rarrow|
``MPI_COMM_GET_ERRHANDLER``.

All parameter types stayed the same.

.. _label-mpi-errhandler-set:

Stop using MPI_ERRHANDLER_SET
-----------------------------

In both C and Fortran, the only thing that changed with regards to
``MPI_ERRHANDLER_SET`` is the name: ``MPI_ERRHANDLER_SET`` |rarrow|
``MPI_COMM_SET_ERRHANDLER``.

All parameter types stayed the same.

.. _label-mpi-type-hindexed:

Stop using MPI_TYPE_HINDEXED
----------------------------

In both C and Fortran, *effectively* the only change is the name of
the function: ``MPI_TYPE_HINDEXED`` |rarrow|
``MPI_TYPE_CREATE_HINDEXED``.

In C, the new function also has a ``const`` attribute on the two array
parameters, but most applications won't notice the difference.

All other parameter types stayed the same.

.. code-block:: c++

    int count = 2;
    int block_lengths[] = { 1, 2 };
    MPI_Aint displacements[] = { 0, sizeof(int) };
    MPI_Datatype newtype;

    // Old way
    MPI_Type_hindexed(count, block_lengths, displacements, MPI_INT, &newtype);

    // New way
    MPI_Type_create_hindexed(count, block_lengths, displacements, MPI_INT, &newtype);

.. _label-mpi-type-hvector:

Stop using MPI_TYPE_HVECTOR
---------------------------

In both C and Fortran, the only change is the name of the function:
``MPI_TYPE_HVECTOR`` |rarrow| ``MPI_TYPE_CREATE_HVECTOR``.

All parameter types stayed the same.

.. _label-mpi-type-struct:

Stop using MPI_TYPE_STRUCT
--------------------------

In both C and Fortran, *effectively* the only change is the name of
the function: ``MPI_TYPE_STRUCT`` |rarrow| ``MPI_TYPE_CREATE_STRUCT``.

In C, the new function also has a ``const`` attribute on the three
array parameters, but most applications won't notice the difference.

All other parameter types stayed the same.

.. code-block:: c++

    int count = 2;
    int block_lengths[] = { 1, 2 };
    MPI_Aint displacements[] = { 0, sizeof(int) };
    MPI_Datatype datatypes[] = { MPI_INT, MPI_DOUBLE };
    MPI_Datatype newtype;

    // Old way
    MPI_Type_struct(count, block_lengths, displacements, datatypes, &newtype);

    // New way
    MPI_Type_create_struct(count, block_lengths, displacements, datatypes, &newtype);

.. _label-mpi-type-extent:

Stop using MPI_TYPE_EXTENT
--------------------------

In both C and Fortran, the ``MPI_TYPE_EXTENT`` function is superseded
by the slightly-different ``MPI_TYPE_GET_EXTENT`` function: the new
function also returns the lower bound.

.. code-block:: c++

    MPI_Aint lb;
    MPI_Aint extent;

    // Old way
    MPI_Type_extent(MPI_INT, &extent);

    // New way
    MPI_Type_get_extent(MPI_INT, &lb, &extent);

.. _label-mpi-type-lb:

Stop using MPI_TYPE_LB
----------------------

In both C and Fortran, the ``MPI_TYPE_LB`` function is superseded by
the slightly-different ``MPI_TYPE_GET_EXTENT`` function: the new
function also returns the extent.

.. code-block:: c++

    MPI_Aint lb;
    MPI_Aint extent;

    // Old way
    MPI_Type_lb(MPI_INT, &lb);

    // New way
    MPI_Type_get_extent(MPI_INT, &lb, &extent);

.. _label-mpi-type-ub:

Stop using MPI_TYPE_UB
----------------------

In both C and Fortran, the ``MPI_TYPE_UB`` function is superseded by
the slightly-different ``MPI_TYPE_GET_EXTENT`` function: the new
function returns the lower bound and the extent, which can be used to
compute the upper bound.

.. code-block:: c++

    MPI_Aint lb, ub;
    MPI_Aint extent;

    // Old way
    MPI_Type_ub(MPI_INT, &ub);

    // New way
    MPI_Type_get_extent(MPI_INT, &lb, &extent);
    ub = lb + extent

Note the ``ub`` calculation after calling ``MPI_Type_get_extent()``.

.. _label-mpi-lb-ub:

Stop using MPI_LB / MPI_UB
--------------------------

The ``MPI_LB`` and ``MPI_UB`` positional markers were fully replaced
with ``MPI_TYPE_CREATE_RESIZED`` in MPI-2.0.

Prior to MPI-2.0, ``MPI_UB`` and ``MPI_LB`` were intended to be used
as input to ``MPI_TYPE_STRUCT`` (which, itself, has been deprecated
and renamed to ``MPI_TYPE_CREATE_STRUCT``).  The same end effect can
now be achieved with ``MPI_TYPE_CREATE_RESIZED``.  For example, using
the old method:

.. code-block:: c++

    int count = 3;
    int block_lengths[] = { 1, 1, 1 };
    MPI_Aint displacements[] = { -2, 0, 10 };
    MPI_Datatype datatypes[] = { MPI_LB, MPI_INT, MPI_UB };
    MPI_Datatype newtype;

    MPI_Type_struct(count, block_lengths, displacements, datatypes, &newtype);
    MPI_Type_commit(&newtype);

    MPI_Aint ub, lb, extent;
    MPI_Type_lb(newtype, &lb);
    MPI_Type_ub(newtype, &ub);
    MPI_Type_extent(newtype, &extent);
    printf("OLD: LB=%d, UB=%d, extent=%d\n",
           lb, ub, extent);

If we run the above, we get an output of:

.. code-block::

    OLD: LB=-2, UB=10, extent=12

The ``MPI_TYPE_RESIZED`` function allows us to take any arbitrary
datatype and set the lower bound and extent directly (which indirectly
sets the upper bound), without needing to setup the arrays and
computing the displacements necessary to invoke
``MPI_TYPE_CREATE_STRUCT``.

Aside from the ``printf`` statement, the following example is exactly
equivalent to the prior example (:ref:`see the MPI_TYPE_UB section
<label-mpi-type-ub>` for a mapping of ``MPI_TYPE_UB`` to
``MPI_TYPE_GET_EXTENT``):

.. code-block:: c++

    MPI_Datatype newtype;

    MPI_Type_create_resized(MPI_INT, -2, 12, &newtype);
    MPI_Type_commit(&newtype);

    MPI_Aint ub, lb, extent;
    MPI_Type_get_extent(newtype, &lb, &extent);
    ub = lb + extent;
    printf("NEW: LB=%d, UB=%d, extent=%d\n",
           lb, ub, extent);

If we run the above, we get an output of:

.. code-block::

    NEW: LB=-2, UB=10, extent=12

.. _label-mpi-combiner-fortran-integers:

Stop using MPI_COMBINER_HINDEXED_INTEGER, MPI_COMBINER_HVECTOR_INTEGER, and MPI_COMBINER_STRUCT_INTEGER
-------------------------------------------------------------------------------------------------------

The ``MPI_COMBINER_HINDEXED_INTEGER``,
``MPI_COMBINER_HVECTOR_INTEGER``, and ``MPI_COMBINER_STRUCT_INTEGER``
constants could previously be returned from ``MPI_TYPE_GET_ENVELOPE``.

Starting with MPI-3.0, these values will never be returned.  Instead,
they will just return the same names, but without the ``_INTEGER``
suffix.  Specifically:

* ``MPI_COMBINER_HINDEXED_INTEGER`` |rarrow| ``MPI_COMBINER_HINDEXED``
* ``MPI_COMBINER_HVECTOR_INTEGER`` |rarrow| ``MPI_COMBINER_HVECTOR``
* ``MPI_COMBINER_STRUCT_INTEGER`` |rarrow| ``MPI_COMBINER_STRUCT``

If your Fortran code is using any of the ``_INTEGER``-suffixed names,
you can just delete the ``_INTEGER`` suffix.

.. _label-mpi-handler-function:

Stop using MPI_Handler_function
-------------------------------

The ``MPI_Handler_function`` C type is only used in the
deprecated/removed function ``MPI_Errhandler_create()``, as described
:ref:`in the MPI_ERRHANDLER_CREATE section
<label-mpi-errhandler-create>`.

Most MPI applications likely won't use this type at all.  But if they
do, they can simply use the new, exactly-equivalent type name (i.e.,
the return type, number, and type of parameters didn't change):
``MPI_Comm_errhandler_function``.

.. code-block:: c++

    void my_errhandler_function(MPI_Comm *comm, int *code, ...)
    {
        // Do something useful to handle the error
    }

    void some_function(void)
    {
        // Old way
        MPI_Handler_function *old_ptr = my_errhandler_function;

        // New way
        MPI_Comm_errhandler_function *new_ptr = my_errhandler_function;
    }

The ``MPI_Handler_function`` type isn't used at all in the Fortran
bindings.

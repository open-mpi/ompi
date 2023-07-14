.. _label-deprecated-functions:

Deprecation warnings while compiling MPI applications
=====================================================

If you see deprecation warnings when compiling MPI applications, it is
because your application is symbols / functions that are deprecated in
MPI. For example:

.. code-block:: sh

    shell$ mpicc deprecated-example.c -c
    deprecated-example.c: In function 'foo':
    deprecated-example.c:6:5: warning: 'MPI_Attr_delete' is deprecated: MPI_Attr_delete was deprecated in MPI-2.0; use MPI_Comm_delete_attr instead [-Wdeprecated-declarations]
         MPI_Attr_delete(MPI_COMM_WORLD, 2);
         ^~~~~~~~~~~~~~~
    In file included from deprecated-example.c:2:
    /usr/local/openmpi/include/mpi.h:2601:20: note: declared here
     OMPI_DECLSPEC  int MPI_Attr_delete(MPI_Comm comm, int keyval)
                        ^~~~~~~~~~~~~~~

Note that the deprecation compiler warnings tells you how to upgrade
your code to avoid the deprecation warnings.  In this example, it
advises you to use ``MPI_Comm_delete_attr()`` instead of
``MPI_Attr_delete()``.

Also, note that even if Open MPI was configured with
``--enable-mpi1-compatibility`` to re-enable removed MPI-1 symbols,
you will still get compiler warnings when you use the removed symbols.

The following is a list of functions that have been deprecated in MPI,
and the function that is replacing them. Some functions have been
deprecated and removed from the MPI specification, these functions are
listed :ref:`here <label-removed-mpi-constructs>`.

.. list-table::
    :header-rows: 1

    * - Deprecated symbol

        (click for more details, below)
      - Replaced with

        (click to go to the corresponding man page)
      - MPI version deprecating the symbol

    * - :ref:`MPI_KEYVAL_CREATE <label-mpi-keyval-create>`
      - :ref:`MPI_COMM_CREATE_KEYVAL <mpi_comm_create_keyval>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_KEYVAL_FREE <label-mpi-keyval-free>`
      - :ref:`MPI_COMM_FREE_KEYVAL <mpi_comm_free_keyval>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_COPY_FUNCTION <label-mpi-copy-delete-function>`
      - :ref:`MPI_COMM_COPY_ATTR_FUNCTION <mpi_comm_create_keyval>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_DELETE_FUNCTION <label-mpi-copy-delete-function>`
      - :ref:`MPI_COMM_DELETE_ATTR_FUNCTION <mpi_comm_create_keyval>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_ATTR_PUT <label-mpi-attr-put>`
      - :ref:`MPI_COMM_SET_ATTR <mpi_comm_set_attr>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_ATTR_GET <label-mpi-attr-get>`
      - :ref:`MPI_COMM_GET_ATTR <mpi_comm_get_attr>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_ATTR_DELETE <label-mpi-attr-delete>`
      - :ref:`MPI_COMM_DELETE_ATTR <mpi_comm_delete_attr>`
      - MPI-2.0 (1996)

    * - :ref:`MPI_Comm_errhandler_fn <label-errhandler-fn>`
      - :ref:`MPI_Comm_errhandler_function <mpi_comm_create_errhandler>`
      - MPI-2.2 (2009)

    * - :ref:`MPI_File_errhandler_fn <label-errhandler-fn>`
      - :ref:`MPI_File_errhandler_function <mpi_file_create_errhandler>`
      - MPI-2.2 (2009)

    * - :ref:`MPI_Win_errhandler_fn <label-errhandler-fn>`
      - :ref:`MPI_Win_errhandler_function <mpi_win_create_errhandler>`
      - MPI-2.2 (2009)

    * - :ref:`MPI_INFO_GET <label-mpi-info-get>`
      - :ref:`MPI_INFO_GET_STRING <mpi_info_get_string>`
      - MPI-4.0 (2021)

    * - :ref:`MPI_INFO_GET_VALUELEN <label-mpi-info-get-valuelen>`
      - :ref:`MPI_INFO_GET_STRING <mpi_info_get_string>`
      - MPI-4.0 (2021)

    * - :ref:`MPI_SIZEOF <label-mpi-sizeof>`
      -  Fortran intrinsics``c_sizeof`` or ``storage_size``
      - MPI-4.0 (2021)

.. _label-mpi-keyval-create:

MPI_Keyval_create
-----------------

``MPI_Keyval_create`` has been deprecated and replaced by
``MPI_Comm_create_keyval``. The C binding of the new function is
identical to the deprecated version. Hence, applications can simply
replace the function that is being invoked.

The Fortran binding differs in that the ``extra_state`` argument is an
address-sized integer in the new interfaces (vs. a regular integer in
the old interfaces). Also, the copy and delete callback functions have
Fortran bindings that are consistent with address-sized attributes.

.. code-block:: Fortran

    USE mpi
    EXTERNAL my_copy_attr_function
    EXTERNAL my_copy_delete_function
    INTEGER ierror
    INTEGER comm_keyval
    INTEGER old_extra_state
    INTEGER(KIND=MPI_ADDRESS_KIND) new_extra_state

    ! Old way
    CALL MPI_KEYVAL_CREATE(my_copy_attr_function, my_copy_delete_function,
                           comm_keyval, old_extra_state, ierror)

    ! New way
    CALL MPI_COMM_CREATE_KEYVAL(my_copy_attr_function, my_delete_attr_function,
                                comm_keyval, new_extra_state, ierror)


.. _label-mpi-keyval-free:

MPI_Keyval_free
----------------

The binding of ``MPI_Keyval_free`` and ``MPI_Comm_free_keyval`` are identical
for both C and Fortran. Users can directly replace the deprecated function with its
new version.

.. _label-mpi-copy-delete-function:

MPI_Copy_function and MPI_Delete_function
------------------------------------------

The ``MPI_Copy_function`` and ``MPI_Delete_function`` are only used in the
deprecated function ``MPI_Keyval_create()``, as described in the
:ref:`MPI_COMM_CREATE_KEYVAL <label-mpi-keyval-create>`.

For C codes, developers can simply use the new, exactly-equivalent
type name (i.e., the return type, number, and type of parameters
didn't change) ``MPI_Comm_copy_attr_function``, and
``MPI_Comm_delete_attr_function`` respectively.

For Fortran applications, the only difference lies in required integer type for the
``extra_state`` argument, which now has to be an address-sized integer.

.. _label-mpi-attr-put:

MPI_Attr_put
------------

The C binding for the deprecated ``MPI_Attr_put`` is identical to its
replacement, ``MPI_Comm_set_attr``.  The Fortran binding differ in the
usage of an addressed size integer for the attribute value in the new
``MPI_Comm_set_attr`` vs. a regular integer in ``MPI_Attr_put``.

.. code-block:: Fortran

    USE mpi
    INTEGER ierror
    INTEGER comm_keyval
    INTEGER old_attr_val
    INTEGER(KIND=MPI_ADDRESS_KIND) new_attr_val

    ! Old way
    CALL MPI_ATTR_PUT(MPI_COMM_WORLD, comm_keyval,
                       old_attr_val, ierror)

    ! New way
    CALL MPI_COMM_SET_ATTR(MPI_COMM_WORLD, comm_keyval,
	                   new_attr_val, ierror)

.. _label-mpi-attr-get:

MPI_Attr_get
------------

The C bindings of the old and the new interfaces are identical.
Fortran binding differ in the usage of an addressed size integer for
the attribute value in the new ``MPI_Comm_get_attr`` vs. a regular
integer in ``MPI_Attr_get``.

.. _label-mpi-attr-delete:

MPI_Attr_delete
---------------

C and Fortran bindings are identical for ``MPI_Attr_delete`` and
``MPI_Comm_delete_attr``, hence developers should be able to just
directly substitute one function call by the other.


.. _label-mpi-info-get:

MPI_Info_get
------------

Applications should replace the use of ``MPI_Info_get`` with ``MPI_Info_get_string``,
but the usage differs slightly. See the example below.

.. code-block:: c++

        MPI_Info info;

	// Create an info object using MPI_Info_create()
	...

	// Retrieve the the value of a provided key later in the code
	char key[] = "my_key";
	char value[64];
        int valuelen=64;
	int flag;

	// Old way
        MPI_Info_get(info, key, valuelen, &value, &flag);

        // New way
	// Note that we pass the address of valuelen with
	// the new interfaces, since the variable will
	// contain the length of the value string after
	// the function call.
        MPI_Info_get_string(info, key, &valuelen, &value, &flag);
    }

.. _label-mpi-info-get-valuelen:

MPI_Info_get_valuelen
---------------------

``MPI_Info_get_valuelen`` has been deprecated since the new function
``MPI_Info_get_string`` also returns the length of the value string.
Please refer to the example shown in :ref:`MPI_INFO_GET <label-mpi-info-get>`.

.. _label-mpi-sizeof:

MPI_Sizeof
----------

The ``MPI_SIZEOF`` construct in Fortran has been deprected since there
are standard Fortran language constructs such as ``c_sizeof`` and
``storage_size`` that can be used instead.

.. _label-errhandler-fn:

MPI_Comm_errhandler_fn, MPI_File_errhandler_fn, MPI_Win_errhandler_fn
---------------------------------------------------------------------

The following function typedefs have been deprecated and are superseded by new
names. Other than the typedef names, the function signatures are exactly the same; the
names were updated to match conventions of other function typedef names.

* ``MPI_Comm_errhandler_fn`` |rarrow| ``MPI_Comm_errhandler_function``
* ``MPI_File_errhandler_fn`` |rarrow| ``MPI_File_errhandler_function``
* ``MPI_Win_errhandler_fn`` |rarrow| ``MPI_Win_errhandler_function``

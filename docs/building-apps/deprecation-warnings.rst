Deprecation warnings while compiling MPI applications
=====================================================

If you see deprecation warnings when compiling MPI applications, it is
because your application is symbols / functions that are deprecated in
MPI.  For example:

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
For example:

.. code-block:: sh

    shell$ mpicc deleted-example.c -c
    deleted-example.c: In function 'foo':
    deleted-example.c:8:5: warning: 'MPI_Address' is deprecated: MPI_Address was removed in MPI-3.0; use MPI_Get_address instead. [-Wdeleted-declarations]
         MPI_Address(buffer, &address);
         ^~~~~~~~~~~
    In file included from deleted-example.c:2:
    /usr/local/openmpi/include/mpi.h:2689:20: note: declared here
     OMPI_DECLSPEC  int MPI_Address(void *location, MPI_Aint *address)
                        ^~~~~~~~~~~

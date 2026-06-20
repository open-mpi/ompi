.. _mpi_t_event_get_num:


MPI_T_event_get_num
===================

.. include_body

:ref:`MPI_T_event_get_num` |mdash| Query the number of MPI_T event types

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_get_num.rst

OUTPUT PARAMETERS
-----------------

* ``num_events``: Returns number of event types


DESCRIPTION
-----------

:ref:`MPI_T_event_get_num` can be used to query the current number of MPI_T event types.

The number of available event types can be queried with a call to
:ref:`MPI_T_event_get_num`. An MPI implementation is allowed to increase the number of event types during the
execution of an MPI process. However, MPI implementations are not allowed to change the
index of an event type or to delete an event type once it has been made visible to the user.

.. note:: The set of event types Open MPI exports depends on the platform and
   on MCA parameters. With all built-in producers disabled (the MCA parameter
   ``mca_base_event_register_producers`` set to 0) the count is 0.


DISCOVERING AVAILABLE EVENTS
----------------------------

Open MPI registers a set of event types and sources, such as communicator and
session lifecycle, error-handler invocations, and (where the OS supports it)
memory releases. To list the registered sources and event types from the
command line, use:

.. code-block:: sh

   ompi_info --event

To enumerate them programmatically, query the count and then each event's
metadata with :ref:`MPI_T_event_get_info`:

.. code-block:: c

   #include <mpi.h>
   #include <stdio.h>

   int main(void) {
       int provided, i, num = 0;
       MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
       MPI_T_event_get_num(&num);
       for (i = 0; i < num; ++i) {
           char name[256];
           int name_len = sizeof(name), num_elements = 0, verbosity, bind;
           MPI_T_event_get_info(i, name, &name_len, &verbosity, NULL, NULL,
                                &num_elements, NULL, NULL, NULL, NULL, &bind);
           printf("event %d: %s (%d elements)\n", i, name, num_elements);
       }
       MPI_T_finalize();
       return 0;
   }


ERRORS
------

:ref:`MPI_T_event_get_num` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface is not
  initialized

.. seealso::
   * :ref:`MPI_T_Events`
   * :ref:`MPI_T`

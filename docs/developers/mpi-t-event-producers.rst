Writing an MPI_T event producer
===============================

The MPI_T events interface (MPI-5.0 section 15.3.8) lets tools subscribe to
discrete, typed events that Open MPI raises at run time. The back end is the
OPAL ``mca_base_event`` framework (``opal/mca/base/mca_base_event.h``), which
mirrors ``mca_base_pvar``. A *producer* is any piece of Open MPI that registers
an event *source* and one or more event *types* and raises them. This page is a
short how-to; the authoritative design is in ``specs/mpi-t-events/spec.md``.

The model
---------

* An **event source** is a clock domain plus some metadata. Every event type
  belongs to exactly one source (the v1 single-source invariant).
* An **event type** has a name, a description, and a fixed *element layout* (an
  ordered list of typed fields the payload carries), like a struct definition.
* A **raise** delivers one event instance, carrying a payload that matches the
  type's element layout, to every subscribed tool callback.

Registering a source and an event type
---------------------------------------

Register the source, then the event type(s) on it. Do this once, from a
registration entry point that is reachable when MCA parameters are registered
(the in-tree producers register from ``ompi_mpit_register_events()``).
Registration is idempotent by name, so it is safe to call more than once.

.. code-block:: c

   #include "opal/mca/base/mca_base_event.h"

   static mca_base_event_t *my_event = NULL;

   void my_producer_register(void)
   {
       mca_base_event_source_t *source;
       /* {value:int32} -- a single 4-byte element at offset 0. */
       const mca_base_var_type_t types[1] = {MCA_BASE_VAR_TYPE_INT32_T};
       const ptrdiff_t offsets[1] = {0};
       int si, ei;

       si = mca_base_event_source_register("ompi.mything",
                                           "My subsystem events",
                                           MCA_BASE_EVENT_SOURCE_ORDERED,
                                           1000000000 /* ticks/sec */,
                                           OPAL_COUNT_MAX /* max_ticks */,
                                           NULL /* default clock */,
                                           true /* allow ad-hoc timestamp reads:
                                                   the default clock is freely
                                                   readable */,
                                           NULL);
       if (0 > si || OPAL_SUCCESS != mca_base_event_source_get_by_index(si, &source)) {
           return;
       }
       /* The exported event-type name is "ompi.mything_base_happened" -- see
          the note on the "ompi." prefix below. */
       ei = mca_base_event_register("ompi", "mything", "base", "happened",
                                    "My thing happened", OPAL_INFO_LVL_4,
                                    1, types, offsets, NULL,
                                    MCA_BASE_VAR_BIND_NO_OBJECT, 0, source, NULL);
       if (0 <= ei) {
           (void) mca_base_event_get_by_index(ei, &my_event);
       }
   }

Event *source* names are taken verbatim from the caller (so this source is named
``ompi.mything``).  Event *type* names, by contrast, are always exported under
the ``ompi.`` namespace: ``mca_base_event_register()`` builds the name from the
``framework``/``component``/``name`` arguments and forces an ``ompi.`` prefix
regardless of the ``project`` argument, so the example above produces
``ompi.mything_base_happened``.  Name your source ``ompi.*`` too, as here, so a
producer's source and its event types share one namespace.

Use exact-width element types (``MCA_BASE_VAR_TYPE_INT32_T``,
``..._UINT64_T``, ``..._DOUBLE``, etc.), not ``SIZE_T``, so the payload has a
portable, well-defined layout. Element offsets must be ascending and start at
0; the computed buffer size must not exceed ``MCA_BASE_EVENT_MAX_PAYLOAD``.

Raising an event
----------------

At the point the event occurs, build the payload to match the element layout
and raise it. The event-type handle is ``NULL`` until registration succeeds, so
NULL-check it.

.. code-block:: c

   if (NULL != my_event) {
       int32_t value = compute_value();
       mca_base_event_raise(my_event, my_event->source, &value);
   }

``mca_base_event_raise()`` returns immediately when no tool is listening (a
single relaxed atomic read plus a predicted branch), so a raise site costs
almost nothing when unused. If building the payload is itself expensive, guard
it with ``mca_base_event_active(my_event)``.

Binding an event to a specific object
-------------------------------------

Most events are ``MCA_BASE_VAR_BIND_NO_OBJECT``: a single tool registration sees
every instance. An event type can instead be *bound* to a class of MPI object,
so a tool subscribes per-object (for example, "tell me when *this* communicator
is named"). Register such an event with the matching ``MCA_BASE_VAR_BIND_*``
value (these mirror the public ``MPI_T_BIND_*`` ordering, so the framework
reports the binding to tools verbatim):

.. code-block:: c

   my_event = register_bound_event("mpi.communicator_name_set", "...",
                                   OPAL_INFO_LVL_4, MCA_BASE_VAR_BIND_MPI_COMM,
                                   1, types, offsets, source);

At the raise site, use ``mca_base_event_raise_bound()`` and pass the *internal
object identity* (the same pointer the tool's handle resolves to -- e.g. the
``ompi_communicator_t *``). Only registrations the tool bound to that object are
notified:

.. code-block:: c

   if (NULL != my_event) {
       struct { uint64_t handle; } payload = { (uint64_t) (uintptr_t) comm };
       mca_base_event_raise_bound(my_event, NULL, comm, &payload);
   }

Restrict bound events to the same non-performance-critical paths as the other
producers (object lifecycle, environmental/config calls), since binding adds a
per-registration comparison on the dispatch path. Keep the payload to the
minimum the callback needs: the tool already knows the bound object, and can
query further detail (such as the new name) from it directly. See section 6.1 of
the specification.

The producer contract
---------------------

* **Never hold the MPI_T big lock (or, in OMPI, raise while holding it).** A
  tool callback may call back into the MPI_T interface, which would deadlock. A
  debug build asserts that no event is raised while ``ompi_mpit_big_lock`` is
  held.
* **Raise from a normal thread context.** v1 delivers callbacks synchronously
  at ``C in {NONE, MPI_RESTRICTED, THREAD_SAFE}``; it does not deliver from a
  signal handler or other async-signal context.
* **Keep the payload fixed-size and matching the registered layout.** The raise
  path copies it into preallocated storage; it never allocates on the hot path.
* **Gate registration on availability.** If a producer depends on an optional
  facility (for example the OPAL memory hooks), register the source/event only
  when it is present; an absent event is conformant (a smaller event set).

A producer that cannot safely deliver from its raise context -- for example the
OS memory-release producer behind ``ompi.mca.memory.patcher.released``, whose
hook runs inside ``free()`` / ``munmap()`` where it may not lock or allocate --
can instead accumulate its occurrences in a lock-free sink and let the framework
*drain* that sink at a later safe operation, delivering one aggregated,
data-bearing event (a registration with only a dropped handler observes each
aggregated batch as a single drop). See section 7.1 of the specification.

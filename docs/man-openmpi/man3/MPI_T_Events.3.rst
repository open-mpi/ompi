.. _mpi_t_events:


MPI_T_Events
============

.. include_body

Open MPI's MPI_T events - overview and the built-in event sources and types

DESCRIPTION
-----------

The MPI tool information interface ("MPI_T") events facility (MPI-5.0 §15.3.8)
lets a tool register callback functions that the MPI implementation invokes
when it *raises* an event. Each event instance carries a timestamp, originates
from a *source* (a clock and ordering domain), and conveys a typed data
payload. See :ref:`MPI_T` for the broader MPI_T interface and
:ref:`MPI_T_init_thread` to initialize it.

Open MPI implements this facility and ships a set of built-in event sources and
event types, described below.

.. note:: The sources and event types listed on this page are an **initial set**
   -- implemented in large part as worked examples and to exercise Open MPI's
   MPI_T events infrastructure -- and are **not exhaustive**. They may be only a
   subset of what a given Open MPI installation exposes: the exported set depends
   on the platform, the Open MPI version, and MCA parameters, and more sources
   and event types may be added over time. **Do not hard-code these names**;
   discover the actual set at run time. From a program, enumerate with the MPI_T
   API
   (:ref:`MPI_T_source_get_num` / :ref:`MPI_T_source_get_info`, and
   :ref:`MPI_T_event_get_num` / :ref:`MPI_T_event_get_info` /
   :ref:`MPI_T_event_get_index`). From the command line, list them with
   ``ompi_info --event``.

Scope of the current implementation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Event delivery is synchronous and occurs only in normal (non-signal) execution
contexts. A callback registered at the
``MPI_T_CB_REQUIRE_ASYNC_SIGNAL_SAFE`` safety level is accepted but is never
selected for delivery in this release. Raising an event is near-free when no
tool is listening (a single atomic read plus a predicted branch), so the
built-in producers impose negligible overhead on applications that do not use
MPI_T.

Discovering and controlling the built-in events
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

List the registered sources and event types on an installation with:

.. code:: sh

   shell$ ompi_info --event

Event types are filtered by verbosity with ``--level``: the built-in types
register at levels 2 through 4. A plain ``ompi_info --event`` shows all of them
(``--event`` defaults to showing every event-type level); pass ``--level`` to
narrow the set. Event sources are always listed, regardless of ``--level``. Add ``--parsable`` for
machine-readable output.

The MCA parameter ``mca_base_event_register_producers`` is the master switch
for all built-in producers (default: enabled). When set to 0, no sources or
event types are registered, which exercises (and is a valid, conformant) empty
event set.

RECEIVING EVENTS
----------------

A tool receives events by registering a callback on an event type. The typical
sequence is:

#. Initialize MPI_T with :ref:`MPI_T_init_thread`.
#. Locate the event type, either by name with :ref:`MPI_T_event_get_index` or
   by iterating 0..N-1 from :ref:`MPI_T_event_get_num` and inspecting each with
   :ref:`MPI_T_event_get_info`.
#. Allocate a *registration handle* with :ref:`MPI_T_event_handle_alloc`. All
   built-in event types use ``MPI_T_BIND_NO_OBJECT``, so pass a NULL object
   handle.
#. Attach a callback with :ref:`MPI_T_event_register_callback`, choosing a
   *callback safety level* (``MPI_T_CB_REQUIRE_NONE``,
   ``MPI_T_CB_REQUIRE_MPI_RESTRICTED``, or ``MPI_T_CB_REQUIRE_THREAD_SAFE``;
   see *Scope* above for the ``ASYNC_SIGNAL_SAFE`` limitation). Optionally,
   register a dropped-event handler with
   :ref:`MPI_T_event_set_dropped_handler`.
#. The implementation invokes the callback whenever it raises the event. Inside
   the callback, read the payload element by element with
   :ref:`MPI_T_event_read` (or copy it whole with :ref:`MPI_T_event_copy`),
   obtain the originating source with :ref:`MPI_T_event_get_source`, and the
   timestamp with :ref:`MPI_T_event_get_timestamp`.
#. Tear down with :ref:`MPI_T_event_handle_free`, then
   :ref:`MPI_T_finalize`.

For example, to observe communicator creation:

.. code:: c

   void my_cb(MPI_T_event_instance event, MPI_T_event_registration reg,
              MPI_T_cb_safety cb_safety, void *user_data)
   {
       int32_t size;
       uint32_t context_id;
       int source_index;
       MPI_Count timestamp;

       MPI_T_event_read(event, 0, &size);        /* element 0: communicator size */
       MPI_T_event_read(event, 1, &context_id);  /* element 1: context id        */
       MPI_T_event_get_source(event, &source_index);
       MPI_T_event_get_timestamp(event, &timestamp);
       /* ... record the observation ... */
   }

   /* ... elsewhere, during setup ... */
   int provided, index;
   MPI_T_event_registration reg;

   MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
   if (MPI_SUCCESS == MPI_T_event_get_index("ompi.mpi.communicator_created", &index)) {
       MPI_T_event_handle_alloc(index, NULL, MPI_INFO_NULL, &reg);
       MPI_T_event_register_callback(reg, MPI_T_CB_REQUIRE_THREAD_SAFE,
                                     MPI_INFO_NULL, NULL, my_cb);
       /* ... run the application; my_cb fires as communicators are created ... */
       MPI_T_event_handle_free(reg, NULL, NULL);
   }
   MPI_T_finalize();

EVENT SOURCES
-------------

All built-in event types are split across just two sources, distinguished by
the one property that differs between them: *ordering*. Both use Open MPI's
default event clock -- the same underlying system clock that backs
:ref:`MPI_Wtime` (``ticks_per_second`` is 1,000,000,000, i.e. nanoseconds, and
``max_ticks`` is the largest value representable in an ``MPI_Count``) -- and
both support *ad-hoc* timestamp reads via :ref:`MPI_T_source_get_timestamp`.
Query these properties at run time with :ref:`MPI_T_source_get_info`.

``ompi``
   Ordered (``MPI_T_SOURCE_ORDERED``); default clock; supports ad-hoc timestamp
   reads. The domain for essentially all of Open MPI's lifecycle events:
   initialization/finalization, communicator, RMA window, and error-handler
   events. Because the source is ordered, the timestamps of
   successive events from it are monotonically non-decreasing, so a tool can use
   them to order the events.

``ompi.unordered``
   Unordered (``MPI_T_SOURCE_UNORDERED``); default clock; supports ad-hoc
   timestamp reads. **Registered only on platforms whose memory-release hooks
   can report releases** (for example, Linux); absent elsewhere. The domain for
   events whose underlying occurrences are aggregated before delivery, so
   per-occurrence chronology is not preserved -- currently just the OS-level
   memory-release event (``ompi.mca.memory.patcher.released``, below).

Why two sources, and what timestamps mean
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A *source* in MPI_T is fundamentally a clock-and-ordering domain, not a way to
group events by subsystem (event names and MPI_T categories already serve that
purpose). Two sources suffice here because the built-in events differ in
exactly one source-visible property: the ``ompi`` events are individually
timestamped and ordered, whereas the ``ompi.unordered`` memory-release event is
aggregated and therefore unordered.

**Timestamps are only comparable within a single source.** A timestamp obtained
from one source must not be compared against a timestamp from another; only
timestamps that share a source establish an ordering.

.. note:: The event clock and :ref:`MPI_Wtime` read the *same* underlying
   system clock and advance at the same rate, but their values are **not
   directly comparable**: an event timestamp is a 64-bit integer count of
   nanoseconds from a lazily-captured origin, whereas ``MPI_Wtime`` returns
   double-precision seconds from a different origin. The unit (integer
   nanoseconds vs. floating-point seconds) and the zero-point both differ, and
   the MPI standard does not require any relationship between the two.

EVENT TYPES
-----------

Most built-in event types bind to no MPI object: the ``bind`` value returned by
:ref:`MPI_T_event_get_info` is ``MPI_T_BIND_NO_OBJECT``, so a tool passes a NULL
object handle to :ref:`MPI_T_event_handle_alloc` and a single registration
observes every instance. One event type, ``ompi.mpi.communicator_name_set``, is
instead *bound* to a communicator (``bind`` is ``MPI_T_BIND_MPI_COMM``): a tool
binds each registration to one specific communicator and is notified only when
*that* communicator is affected. See `Binding an event to a specific object`_
below. The *element layout* described for each event is the ordered sequence of
typed fields each instance carries, read with :ref:`MPI_T_event_read` or copied
with :ref:`MPI_T_event_copy`. The *source* of an instance can be queried in the
callback with :ref:`MPI_T_event_get_source`. The "level" shown is the MPI_T
verbosity level (on the 1-9 scale used by ``ompi_info --level``).

Binding an event to a specific object
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For an event type whose ``bind`` is not ``MPI_T_BIND_NO_OBJECT``, a tool binds
each registration handle to one specific object: it passes the *address of that
object's handle* as the ``obj_handle`` argument of
:ref:`MPI_T_event_handle_alloc`. The registration is then notified only when the
event occurs for that particular object; the event for any other object of the
same class is not delivered to that registration. (To watch several objects, a
tool allocates one registration per object.) For a ``MPI_T_BIND_NO_OBJECT``
event, ``obj_handle`` is ignored.

The bound object is not passed as a callback argument -- the tool already knows
it, having bound to it, and typically records it in the callback's ``user_data``.
A bound event whose payload would otherwise just echo the bound object instead
carries only the minimal data the callback needs; a callback queries any further
detail directly from the object it bound to.

.. note:: Several payloads carry an MPI object's *handle* (a communicator,
   window, error-handler, or session) as a ``uint64`` element. This is an opaque
   token, reported in the MPI ABI representation of the tool that registered the
   callback, and intended for identifying the object and correlating related
   events (for example pairing a "created" event with its "freed"). Do not
   dereference it. Under the Open MPI ABI the value is the C handle the
   application holds; with the (forthcoming) MPI Standard ABI it is that ABI's
   integer handle.

``ompi.mpi.communicator_created``
   Source ``ompi``; level 2. Raised after Open MPI successfully creates a new
   communicator -- for example via :ref:`MPI_Comm_dup`, :ref:`MPI_Comm_split`,
   :ref:`MPI_Comm_create`, the intercommunicator calls, and the communicators
   Open MPI builds internally -- once the communicator is usable. Payload
   (2 elements):

   * ``size`` (``int32``) -- the new communicator's group size.
   * ``handle`` (``uint64``) -- the ``MPI_Comm`` handle value, an opaque token
     for identifying the communicator and pairing this event with its "freed"
     event; it must not be dereferenced.

``ompi.mpi.communicator_freed``
   Source ``ompi``; level 2. Raised while a communicator is being destroyed (for
   example via :ref:`MPI_Comm_free`, or when Open MPI tears down an internal
   communicator), while its handle is still valid. Payload (1 element):

   * ``handle`` (``uint64``) -- the ``MPI_Comm`` handle value; matching it
     against a "created" event pairs creation with destruction.

``ompi.mpi.communicator_name_set``
   Source ``ompi``; level 4. **Bound to a communicator** (``bind`` =
   ``MPI_T_BIND_MPI_COMM``; see `Binding an event to a specific object`_): a tool
   binds a registration to one communicator and is notified only when *that*
   communicator's name is set with :ref:`MPI_Comm_set_name`. Payload (1 element):

   * ``handle`` (``uint64``) -- the ``MPI_Comm`` handle value (the bound
     communicator); an opaque token that must not be dereferenced.

   The new name itself is not in the payload: a callback reads it with
   :ref:`MPI_Comm_get_name` on the communicator it bound to. Open MPI also names
   some predefined and internal communicators, so a registration bound to such a
   communicator may observe those namings too.

``ompi.mpi.initialization``
   Source ``ompi``; level 2. Raised when an MPI instance has been initialized
   and is ready for use, for **both** MPI models: the world model
   (:ref:`MPI_Init` / :ref:`MPI_Init_thread`) and the session model
   (:ref:`MPI_Session_init`), which run through the same internal instance-init
   path. Payload (5 elements):

   * ``model`` (``int32``) -- the MPI model, as an ``OMPI_T_MODEL_*`` value from
     ``<mpi.h>``: ``OMPI_T_MODEL_WORLD`` or ``OMPI_T_MODEL_SESSION``.
   * ``thread_level`` (``int32``) -- the thread-support level the implementation
     provided (e.g. ``MPI_THREAD_MULTIPLE``).
   * ``world_rank`` (``int32``) -- this process' rank in ``MPI_COMM_WORLD``;
     meaningful only for the world model, and ``-1`` for a session (a process
     has no rank until a communicator is derived from a session).
   * ``world_size`` (``int32``) -- the size of ``MPI_COMM_WORLD``; meaningful
     only for the world model, and ``-1`` for a session.
   * ``instance_id`` (``uint64``) -- an opaque token that correlates this event
     with the matching ``ompi.mpi.finalization`` event for the same instance
     (most useful in the session model, where many instances may coexist); it
     must not be dereferenced.

   The world-model event fires during ``MPI_Init`` once ``MPI_COMM_WORLD``
   exists, so a tool must attach before ``MPI_Init`` to observe it.

``ompi.mpi.finalization``
   Source ``ompi``; level 2. Raised when an MPI instance is about to be
   finalized, for both models: the world model (:ref:`MPI_Finalize`) and the
   session model (:ref:`MPI_Session_finalize`). Payload (3 elements):

   * ``model`` (``int32``) -- the MPI model, as an ``OMPI_T_MODEL_*`` value from
     ``<mpi.h>``: ``OMPI_T_MODEL_WORLD`` or ``OMPI_T_MODEL_SESSION``.
   * ``world_rank`` (``int32``) -- this process' rank in ``MPI_COMM_WORLD`` for
     the world model; ``-1`` for a session.
   * ``instance_id`` (``uint64``) -- correlates with the
     ``ompi.mpi.initialization`` event for the same instance.

``ompi.mpi.errhandler_invoked``
   Source ``ompi``; level 4. Raised each time Open MPI invokes an error handler
   -- that is, whenever an MPI operation detects an error and calls the error
   handler attached to the relevant communicator, window, file, or session
   (including the predefined ``MPI_ERRORS_ARE_FATAL`` / ``MPI_ERRORS_RETURN``) --
   before the handler's effect (such as aborting the job) takes place. Payload
   (4 elements):

   * ``err_code`` (``int32``) -- the MPI error code being raised.
   * ``object_type`` (``int32``) -- which kind of MPI object the handler is
     associated with, as an ``MPI_T_BIND_*`` value: ``MPI_T_BIND_NO_OBJECT``
     (a predefined handler, or none), ``MPI_T_BIND_MPI_COMM``,
     ``MPI_T_BIND_MPI_WIN``, ``MPI_T_BIND_MPI_FILE``, or
     ``MPI_T_BIND_MPI_SESSION``.
   * ``errhandler_handle`` (``uint64``) -- the ``MPI_Errhandler`` handle value.
   * ``object_handle`` (``uint64``) -- the handle of the MPI object the handler
     is invoked on, of the kind given by ``object_type``.

   The two handle values are opaque tokens (each ``0`` when not available -- for
   example when an error is routed to a predefined handler before ``MPI_Init``);
   they must not be dereferenced.

``ompi.mpi.win_created``
   Source ``ompi``; level 4. Raised after Open MPI successfully creates a new
   RMA window, for any window flavor (:ref:`MPI_Win_create`,
   :ref:`MPI_Win_allocate`, :ref:`MPI_Win_allocate_shared`, or
   :ref:`MPI_Win_create_dynamic`), once the window is usable. Payload
   (4 elements):

   * ``size`` (``int64``) -- the local window size in bytes; ``0`` for a
     dynamic window.
   * ``disp_unit`` (``int32``) -- the displacement unit.
   * ``flavor`` (``int32``) -- the ``MPI_WIN_FLAVOR_*`` value identifying how the
     window was created.
   * ``handle`` (``uint64``) -- the ``MPI_Win`` handle value, an opaque token for
     identifying the window and pairing this event with its "freed" event.

``ompi.mpi.win_freed``
   Source ``ompi``; level 4. Raised by :ref:`MPI_Win_free`, before the window is
   torn down. Payload (2 elements):

   * ``flavor`` (``int32``) -- the ``MPI_WIN_FLAVOR_*`` value of the window being
     freed.
   * ``handle`` (``uint64``) -- the ``MPI_Win`` handle value.

``ompi.mca.memory.patcher.released``
   Source ``ompi.unordered``; level 4. Present only where the ``ompi.unordered``
   source is (see above). It reports memory returned to the operating system, as
   observed by Open MPI's memory-release hooks (the "patcher" memory component,
   which intercepts ``munmap``, ``mremap``, ``madvise``, ``brk`` / ``sbrk``
   shrink, and SysV ``shmdt``). Payload (2 elements, both ``uint64``):

   * ``count`` -- the number of release operations aggregated since the previous
     delivery.
   * ``bytes`` -- their cumulative size in bytes.

   This event differs from the others in how it is produced and delivered. The
   hook runs *inside* the intercepted call (e.g. ``munmap``), a context where it
   may not lock, allocate, or walk data structures, so it cannot deliver an
   event there. Instead it accumulates ``count`` and ``bytes`` in a lock-free
   sink, and the framework *drains* that sink and delivers one aggregated event
   at the next safe framework operation -- notably a
   :ref:`MPI_T_source_get_timestamp` read of the ``ompi.unordered`` source (and
   also event-handle allocation and free). A registration that has only a
   dropped-event handler (no callback) instead observes each aggregated batch as
   a single dropped event (see :ref:`MPI_T_event_set_dropped_handler`).

   Two consequences follow from relying on these memory hooks. First, the hooks
   are *chunk-level*: they fire only when memory is actually returned to the OS
   (for example a large ``free()`` that unmaps, or a heap-shrinking ``sbrk``),
   not on every ``free()``. Second, the hook cannot distinguish which call
   caused a release, so the payload is a coarse aggregate. A per-API breakdown,
   and capture of the individual freed addresses, are possible future extensions
   but are intentionally not provided here: they would require extending the
   memory-hook interface and, for addresses, allocating memory in a context
   where allocation is unsafe.

.. seealso::
   * :ref:`MPI_T`
   * :ref:`MPI_T_init_thread`
   * :ref:`MPI_T_source_get_num`
   * :ref:`MPI_T_source_get_info`
   * :ref:`MPI_T_source_get_timestamp`
   * :ref:`MPI_T_event_get_num`
   * :ref:`MPI_T_event_get_info`
   * :ref:`MPI_T_event_get_index`
   * :ref:`MPI_T_event_handle_alloc`
   * :ref:`MPI_T_event_register_callback`
   * :ref:`MPI_T_event_get_source`
   * :ref:`MPI_T_event_read`
   * :ref:`MPI_T_event_set_dropped_handler`

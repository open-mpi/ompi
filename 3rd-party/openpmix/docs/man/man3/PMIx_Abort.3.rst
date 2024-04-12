.. _man3-PMIx_Abort:

PMIx_Abort
==========

PMIx_Abort - Abort the specified processes

SYNOPSIS
--------

.. code-block:: c

   #include <pmix.h>

   pmix_status_t PMIx_Abort(int status, const char msg[],
                            pmix_proc_t procs[], size_t nprocs);

ARGUMENTS
---------

* ``status``: Status value to be returned. A value of zero is
  permitted by PMIx, but may not be returned by some resource
  managers.

* ``msg``: A string message to be displayed.

* ``procs``: An array of ``pmix_proc_t`` structures defining the
  processes to be aborted. A NULL for the proc array indicates that
  all processes in the caller's namespace are to be aborted. A
  wildcard value for the rank in any structure indicates that all
  processes in that namespace are to be aborted.

* ``nprocs``: Number of ``pmix_proc_t`` structures in the ``procs``
  array.

DESCRIPTION
-----------

Request that the provided array of procs be aborted, returning the
provided status and printing the provided message. A NULL for the
``procs`` array indicates that all processes in the caller's namespace
are to be aborted.

The response to this request is somewhat dependent on the specific
resource manager and its configuration (e.g., some resource managers
will not abort the application if the provided status is zero unless
specifically configured to do so), and thus lies outside the control
of PMIx itself. However, the client will inform the RM of the request
that the application be aborted, regardless of the value of the
provided status.

Passing a NULL ``msg`` parameter is allowed. Note that race conditions
caused by multiple processes calling ``PMIx_Abort`` are left to the server
implementation to resolve with regard to which status is returned and
what messages (if any) are printed.

RETURN VALUE
------------

Returns ``PMIX_SUCCESS`` on success. On error, a negative value
corresponding to a PMIx ``errno`` is returned.

ERRORS
------

PMIx errno values are defined in ``pmix_common.h``.

.. JMS COMMENT When more man pages are added, they can be :ref:'ed
   appropriately, so that HTML hyperlinks are created to link to the
   corresponding pages.

.. seealso::
   PMIx_Commit(3),
   :ref:`PMIx_Finalize(3) <man3-PMIx_Finalize>`,
   :ref:`PMIx_Init(3) <man3-PMIx_Init>`,
   PMIx_Initialized(3),
   PMIx_Put(3),
   pmiAddInstance(3),
   pmiAddMetric(3)

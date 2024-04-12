.. _man3-PMIx_Init:

PMIx_Init
=========

PMIx_Init - Initialize the PMIx Client

SYNOPSIS
--------

.. code-block:: c

   #include <pmix.h>

   pmix_status_t PMIx_Init(pmix_proc_t *proc);

ARGUMENTS
---------

* ``proc``: Pointer to a ``pmix_proc_t`` object in which the client's
  namespace and rank are to be returned.

DESCRIPTION
-----------

Initialize the PMIx client, returning the process identifier assigned
to this client's application in the provided ``pmix_proc_t``
struct. Passing a parameter of NULL for this parameter is allowed if
the user wishes solely to initialize the PMIx system and does not
require return of the identifier at that time.

When called, the PMIx client will check for the required connection
information of the local PMIx server and will establish the
connection. If the information is not found, or the server connection
fails, then an appropriate error constant will be returned.

If successful, the function will return ``PMIX_SUCCESS`` and will fill
the provided structure with the server-assigned namespace and rank of
the process within the application.

Note that the PMIx client library is referenced counted, and so
multiple calls to ``PMIx_Init`` are allowed. Thus, one way to obtain
the namespace and rank of the process is to simply call ``PMIx_Init``
with a non-NULL parameter.

RETURN VALUE
------------

Returns ``PMIX_SUCCESS`` on success. On error, a negative value
corresponding to a PMIx ``errno`` is returned.

ERRORS
------

PMIx ``errno`` values are defined in ``pmix_common.h``.

.. JMS COMMENT When more man pages are added, they can be :ref:'ed
   appropriately, so that HTML hyperlinks are created to link to the
   corresponding pages.

.. seealso::
   PMIx_Initialized(3),
   :ref:`PMIx_Abort(3) <man3-PMIx_Abort>`,
   PMIx_Commit(3),
   :ref:`PMIx_Finalize(3) <man3-PMIx_Finalize>`,
   PMIx_Put(3),
   pmiAddInstance(3),
   pmiAddMetric(3)

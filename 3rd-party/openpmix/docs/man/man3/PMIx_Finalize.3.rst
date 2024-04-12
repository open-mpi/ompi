.. _man3-PMIx_Finalize:

PMIx_Finalize
=============

PMIx_Finalize - Finalize the PMIx Client

SYNOPSIS
--------

.. code-block:: c

   #include <pmix.h>

   pmix_status_t PMIx_Finalize(void);

DESCRIPTION
-----------

Finalize the PMIx client, closing the connection with the local PMIx
server.

RETURN VALUE
------------

Returns ``PMIX_SUCCESS`` on success. On error, a negative value
corresponding to a PMIx ``errno`` is returned.

ERRORS
------

PMIx ``errno`` values are defined in ``pmix_common.h``.

.. seealso::
   :ref:`PMIx_Abort(3) <man3-PMIx_Abort>`,
   PMIx_Commit(3),
   :ref:`PMIx_Init(3) <man3-PMIx_Init>`,
   PMIx_Initialized(3),
   PMIx_Put(3),
   pmiAddInstance(3),
   pmiAddMetric(3)

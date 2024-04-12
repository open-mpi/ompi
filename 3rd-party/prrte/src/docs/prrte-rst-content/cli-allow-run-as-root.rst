.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Allow execution as root **(STRONGLY DISCOURAGED)**.

Running as root exposes the user to potentially catastrophic file
system corruption and damage |mdash| e.g., if the user accidentally
points the root of the session directory to a system required point,
this directory and all underlying elements will be deleted upon job
completion, thereby rendering the system inoperable.

It is recognized that some environments (e.g., containers) may require
operation as root, and that the user accepts the risks in those
scenarios. Accordingly, one can override PRRTE's run-as-root
protection by providing one of the following:

* The ``--allow-run-as-root`` command line directive
* Adding **BOTH** of the following environmental parameters:

    * ``PRTE_ALLOW_RUN_AS_ROOT=1``
    * ``PRTE_ALLOW_RUN_AS_ROOT_CONFIRM=1``

Again, we recommend this only be done if absolutely necessary.

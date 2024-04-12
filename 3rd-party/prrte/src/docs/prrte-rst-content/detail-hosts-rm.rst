.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Resource Manager-Provided Hosts
===============================

When launching under a Resource Manager (RM), the RM usually
picks which hosts |mdash| and how many processes can be launched on
each host |mdash| on a per-job basis.

The RM will communicate this information to PRRTE directly; users can
simply omit specifying hosts or numbers of processes.

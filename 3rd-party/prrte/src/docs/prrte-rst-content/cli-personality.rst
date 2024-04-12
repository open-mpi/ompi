.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Specify the personality to be used. This governs selection of the
plugin responsible for defining and parsing the command line,
harvesting and forwarding environmental variables, and providing
library-dependent support to the launched processes. Examples include
``ompi`` for an application compiled with Open MPI, ``mpich`` for one
built against the MPICH library, or ``oshmem`` for an OpenSHMEM
application compiled against SUNY's reference library.

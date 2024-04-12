.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

PRRTE supports several levels of user-specified hostfiles based on an
established precedence order. Users can specify a hostfile that
contains a list of nodes to be used for the job, or can provide a
comma-delimited list of nodes to be used for that job via the
``--host`` command line option.

The precedence order applied to these various options depends to some
extent on the local environment. The following table illustrates how
host and hostfile directives work together to define the set of hosts
upon which a DVM will execute the job in the absence of a resource
manager (RM):

.. list-table::
   :header-rows: 1
   :widths: 7 10 45

   * - host
     - hostfile
     - Result

   * - unset
     - unset
     - | The DVM will utilize all its available resources
       | when mapping the job.

   * - set
     - unset
     - | Host option defines resource list for the job

   * - unset
     - set
     - | Hostfile defines resource list for the job

   * - set
     - set
     - | Hostfile defines resource list for the job,
       | then host filters the list to define the final
       | set of nodes to be used for the job

.. include:: /prrte-rst-content/detail-hostfiles.rst

.. include:: /prrte-rst-content/detail-hosts-relative-indexing.rst

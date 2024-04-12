.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

PRRTE supports several levels of user-specified host lists based on an
established precedence order. Users can specify a default hostfile
that contains a list of nodes to be used by the DVM. Only one default
hostfile can be provided for a given DVM. In addition, users can
specify a hostfile that contains a list of nodes to be used for a DVM,
or can provide a comma-delimited list of nodes to be used for that DVM
via the ``--host`` command line option.

The precedence order applied to these various options depends to some
extent on the local environment. The following table illustrates how
host and hostfile directives work together to define the set of hosts
upon which a DVM will execute in the absence of a resource manager
(RM):

.. list-table::
   :header-rows: 1
   :widths: 10 7 10 32

   * - Default hostfile
     - host
     - hostfile
     - Result

   * - unset
     - unset
     - unset
     - | The DVN will consist solely of the
       | local host where the DVM
       | was started.

   * - unset
     - set
     - unset
     - | Host option defines resource list for the DVM.

   * - unset
     - unset
     - set
     - | Hostfile option defines resource list for the DVM.

   * - unset
     - set
     - set
     - | Hostfile option defines resource list for the DVM,
       | then host filters the list to define the final
       | set of nodes to be used by the DVM

   * - set
     - unset
     - unset
     - | Default hostfile defines resource list for the DVM

   * - set
     - set
     - unset
     - | Default hostfile defines resource list for the DVM,
       | then host filters the list to define the final
       | set of nodes to be used by the DVM

   * - set
     - set
     - set
     - | Default hostfile defines resource list for the DVM,
       | then hostfile filters the list, and then host filters
       | the list to define the final set of nodes to be
       | used by the DVM

This changes somewhat in the presence of an RM as that entity
specifies the initial allocation of nodes. In this case, the default
hostfile, hostfile and host directives are all used to filter the RM's
specification so that a user can utilize different portions of the
allocation for different DVMs. This is done according to the same
precedence order as in the prior table, with the RM providing the
initial pool of nodes.

.. include:: /prrte-rst-content/detail-hostfiles.rst

.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

A required argument is passed to the ``--dvm`` directive to specify
the location of the DVM controller (e.g., ``--dvm pid:12345``) or by
passing the string ``search`` to instead search for an existing
controller.

Supported options include:

* ``search``: directs the tool to search for available DVM controllers
  it is authorized to use, connecting to the first such candidate it
  finds.

* ``pid:<arg>``: provides the PID of the target DVM controller. This
  can be given as either the PID itself (arg = int) or the path to a
  file that contains the PID (arg = ``file:<path>``)

* ``file:<path>``: provides the path to a PMIx rendezvous file that is
  output by PMIx servers |mdash| the file contains all the required
  information for completing the connection

* ``uri:<arg>``: specifies the URI of the DVM controller, or the name of
  the file (specified as ``file:filename``) that contains that info

* ``ns:<arg>``: specifies the namespace of the DVM controller

* ``system``: exclusively find and use the system-level DVM controller

* ``system-first``: look for a system-level DVM controller, fall back
  to searching for an available DVM controller the command is
  authorized to use if a system-level controller is not found

Examples:

.. code::

   prterun --dvm file:dvm_uri.txt --np 4 ./a.out

   prterun --dvm pid:12345 --np 4 ./a.out

   prterun --dvm uri:file:dvm_uri.txt --np 4 ./a.out

   prterun --dvm ns:prte-node1-2095 --np 4 ./a.out

   prterun --dvm pid:file:prte_pid.txt --np 4 ./a.out

   prterun --dvm search --np 4 ./a.out

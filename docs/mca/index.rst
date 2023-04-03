.. _label-mca:

The Modular Component Architecture (MCA)
========================================

Open MPI is a highly-customizable system; it can be configured via
configuration files, command line parameters, and environment
variables.  The main functionality of Open MPI's configuration system
is through the Modular Component Architecture (MCA).

* This section describes the MCA itself and how to set MCA parameters at
  run time.
* Later sections in this documentation describe different parts of
  Open MPI's functionality, and the specific names and values of MCA
  parameters that can be used to affect Open MPI's behavior.

.. note:: :ref:`The PMIx and PRRTE software packages
          <label-running-role-of-pmix-and-prte>` also use the MCA for
          their configuration, composition, and run-time tuning.


.. toctree::
   :maxdepth: 1

   quickstart
   terminology
   set-mca-param
   select-component
   common-mca-param

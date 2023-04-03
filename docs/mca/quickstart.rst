.. _label-mca-quickstart:

Quick start: Run-time tuning Open MPI
=======================================

Although this section skips many details, it offers examples that will
probably work in many environments.

.. caution:: Note that this section is a "Quick start" |mdash| it does
   not attempt to be comprehensive or describe behavior in all
   supported environments. The examples below therefore may not work
   exactly as shown in your environment.

   Please consult the other sections in this chapter for more details,
   if necessary.

Open MPI is highly configurable and can be configured during run-time
using configuration files, command line parameters, and environment
variables. The main configuration system is through the :ref:`Modular
Component Architecture (MCA)<label-mca>`. It is highly advisable to
read the linked section if any terms are unknown.

Setting MCA parameter values
----------------------------

:ref:`MCA parameters<label-mca-terminology-parameters>` (sometimes
called MCA *variables*) can be set by:

#. :ref:`Command line parameters:<label-mca-set-param-command-line>`

   .. code-block:: sh

      shell$ mpirun --mca mpi_show_handle_leaks 1 --np 4 a.out

#. :ref:`Environment variables:<label-mca-set-param-env-var>`

   .. code-block:: sh

      shell$ export OMPI_MCA_mpi_show_handle_leaks=1
      shell$ mpirun --np 4 a.out

#. :ref:`Tuning MCA parameter files:<label-mca-set-param-tune>`

   .. code-block:: sh

      shell$ cat foo.conf
      --mca pml ob1
      --mca pml_base_verbose 100
      shell$ cat bar.conf
      --mca coll tuned
      shell$ mpirun --np 2 --tune foo.conf,bar.conf a.out

Viewing available MCA parameters
--------------------------------

Open MPI has a large number of MCA parameters available. Users can
use the :ref:`ompi_info<man1-ompi_info>` command to see all available MCA parameters.
Note, ompi_info only shows a few common MCA params by default
and you have to specify ``ompi_info --all`` to see all of them.

The vast majority of these MCA parameters, however, are probably only useful
to advanced users and/or developers of Open MPI itself. See the linked section for :doc:`Common MCA parameters
<common-mca-param>`.

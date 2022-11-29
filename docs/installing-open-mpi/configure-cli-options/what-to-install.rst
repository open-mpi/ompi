Deciding what to install
========================

Open MPI's ``configure`` script will, by default, search for support
and build every component that it can.  This is convenient, and
helpful for a good out-of-the-box experience for many HPC
environments.

However, HPC clusters rarely change from day-to-day, and large
clusters rarely change at all.  If you know your cluster's
configuration, there are several steps you can take to both reduce
Open MPI's memory footprint and reduce the launch time of large-scale
applications.  These steps use a combination of build-time
configuration options to eliminate components |mdash| thus eliminating
their libraries and avoiding unnecessary component open/close
operations |mdash| as well as run-time MCA parameters to specify what
modules to use by default for most users.

.. caution:: This is somewhat advanced functionality, and is only
             recomended for users who are deeply familiar with what
             components are actually used by Open MPI in their
             environments.

             Most users should just allow building whatever components
             Open MPI's ``configure`` script finds.

Build/install-time choices
--------------------------

One way to save memory is to avoid building components that will
actually never be selected by the system.  Unless MCA parameters
specify which components to open, installed components are *always*
opened and tested as to whether or not they should be selected for
use. If you know that a component can build on your system, but due to
your cluster's configuration will never actually be selected, then it
is best to simply configure Open MPI to not build that component by
using the ``--enable-mca-no-build`` CLI option to ``configure``.

For example, if you know that your system will only utilize the
``ob1`` component of the PML framework, then you can "no build" all
the others:

.. code:: sh

   # See what directories (i.e., components) in exist in the PML
   # framework
   shell$ ls -1 ompi/mca/pml

   # Do not list "base", but list all other undesired components
   # (i.e., directories).  For example, in Open MPI v5.0.0, to build
   # *only* the OB1 PML:
   shell$ ./configure --enable-mca-no-build=pml-cm,pml-monitoring,pml-ucx,pml-v


This not only reduces the size of the Open MPI libraries,
but also reduces memory footprint that is consumed by Open MPI opening
all the built components to see which of them can be selected to run.

Run-time choices
----------------

The ``$sysconfdir/openmpi-mca-params.conf`` file in the installation
tree (which defaults to ``$prefix/etc/openmpi-mca-params.conf``) is
where a system administrator can set system-wide defaults for Open MPI
:ref:`run-time MCA parameters <label-run-time-tuning>`.

These values can still be overridden by end users, but the values in
this file allow the hiding of any system-specific defaults that an
administrator may want the majority of users to utilize.

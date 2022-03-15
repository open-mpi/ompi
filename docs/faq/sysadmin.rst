System administrator-level technical information
================================================

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

I'm a sysadmin; what do I care about Open MPI?
----------------------------------------------

Several members of the Open MPI team have strong system
administrator backgrounds; we recognize the value of having software
that is friendly to system administrators.  Here are some of the reasons
that Open MPI is attractive for system administrators:

* Simple, standards-based installation
* Reduction of the number of MPI installations
* Ability to set system-level and user-level parameters
* Scriptable information sources about the Open MPI installation

See the rest of the questions in this FAQ section for more details.

/////////////////////////////////////////////////////////////////////////

Do I need multiple Open MPI installations?
------------------------------------------

Yes and no.

Open MPI can handle a variety of different run-time environments
(e.g., ssh, Slurm, PBS, etc.) and a variety of different
interconnection networks (e.g., ethernet, InfiniBand, etc.)
in a single installation.  Specifically: because Open MPI is
fundamentally powered by a component architecture, plug-ins for all
these different run-time systems and interconnect networks can be
installed in a single installation tree.  The relevant plug-ins will
only be used in the environments where they make sense.

Hence, there is no need to have one MPI installation for InfiniBand, one
MPI installation for ethernet, one MPI installation for PBS, one MPI
installation for ``ssh``, etc.  Open MPI can handle all of these in a
single installation.

However, there are some issues that Open MPI cannot solve.  Binary
compatibility between different compilers is such an issue.  Let's
examine this on a per-language basis (be sure see the big caveat at
the end):

* *C:* Most C compilers are fairly compatible, such that if you compile
  Open MPI with one C library and link it to an application that was
  compiled with a different C compiler, everything should "just work."
  As such, a single installation of Open MPI should work for most C MPI
  applications.

* *C++:* The same is not necessarily true for C++.  While Open MPI does not currently contain any C++ code (the MPI C++ bindings were removed in a prior release), and C++ compilers *should* produce ABI-equivalent code for C symbols, obscure problem can sometimes arise when mixing compilers from different suites.  For example, if you compile Open MPI with the XYZ C/C++
  compiler, you may need to have the XYC C++ run-time libraries
  installed everywhere you want to run.

* *Fortran:* There are multiple issues with Fortran.

    #. Fortran compilers do something called "symbol mangling," meaning that the
       back-end symbols may have slightly different names than their corresponding
       global variables, subroutines, and functions.  There are 4 common name
       mangling schemes in use by Fortran compilers.  On many systems (e.g.,
       Linux), Open MPI will automatically support all 4 schemes.  As such, a
       single Open MPI installation *should* just work with multiple different
       Fortran compilers.  However, on some systems, this is not possible (e.g.,
       OS X), and Open MPI will only support the name mangling scheme of the
       Fortran compiler that was identified during ``configure``.

    #. That being said, there are two notable exceptions that do *not* work
       across Fortran compilers that are "different enough":

        #. The C constants ``MPI_F_STATUS_IGNORE`` and ``MPI_F_STATUSES_IGNORE``
             will only compare properly to Fortran applications that were
             created with Fortran compilers that that use the same
             name-mangling scheme as the Fortran compiler with which Open MPI was
             configured.

        #. Fortran compilers may have different values for the logical
             ``.TRUE.`` constant.  As such, any MPI function that uses the
             Fortran ``LOGICAL`` type may only get ``.TRUE.`` values back that
             correspond to the the ``.TRUE.`` value of the Fortran compiler with which
             Open MPI was configured.

    #. Similar to C++, linking object files that Fortran language features such as modules and/or polymorphism from different
       Fortran compilers is not likely to work.  The ``mpi`` and ``mpi_f08`` modules that
       Open MPI creates will likely only work with the Fortran compiler
       that was identified during ``configure`` (and used to build Open MPI).

The big caveat to all of this is that Open MPI will only work with
different compilers *if all the datatype sizes are the same.*  For
example, even though Open MPI supports all 4 name mangling schemes,
the size of the Fortran ``LOGICAL`` type may be 1 byte in some compilers
and 4 bytes in others.  This will likely cause Open MPI to perform
unpredictably.

The bottom line is that Open MPI can support all manner of run-time
systems and interconnects in a single installation, but supporting
multiple compilers "sort of" works (i.e., is subject to trial and
error) in some cases, and definitely does not work in other cases.
There's unfortunately little that we can do about this |mdash| it's a
compiler compatibility issue, and one that compiler authors have
little incentive to resolve.

/////////////////////////////////////////////////////////////////////////

What are MCA Parameters?  Why would I set them?
-----------------------------------------------

MCA parameters are a way to tweak Open MPI's behavior at
run-time.  For example, MCA parameters can specify:

* Which interconnect networks to use
* Which interconnect networks *not* to use
* The size difference between eager sends and rendezvous protocol sends
* How many registered buffers to pre-pin (e.g., for InfiniBand)
* The size of the pre-pinned registered buffers
* ...etc.

It can be quite valuable for a system administrator to play with such
values a bit and find an "optimal" setting for a particular
operating environment.  These values can then be set in a global text
file that all users will, by default, inherit when they run Open MPI
jobs.

For example, say that you have a cluster with 2 ethernet networks |mdash|
one for NFS and other system-level operations, and one for MPI jobs.
The system administrator can tell Open MPI to not use the NFS TCP
network at a system level, such that when users invoke ``mpirun`` or
``mpiexec`` to launch their jobs, they will automatically only be using
the network meant for MPI jobs.

:doc:`See the run-time tuning FAQ category </faq/tuning>` for information on how to set global MCA parameters.

/////////////////////////////////////////////////////////////////////////

Do my users need to have their own installation of Open MPI?
------------------------------------------------------------

Usually not.  It is typically sufficient for a single Open MPI
installation (or perhaps a small number of Open MPI installations,
depending on compiler interoperability) to serve an entire parallel
operating environment.

Indeed, a system-wide Open MPI installation can be customized on a
per-user basis in two important ways:

* *Per-user MCA parameters:* Each user can set their own set of MCA
  parameters, potentially overriding system-wide defaults.
* *Per-user plug-ins:* Users can install their own Open MPI
  plug-ins under ``$HOME/.openmpi/components``.  Hence, developers can
  experiment with new components without destabilizing the rest of the
  users on the system.  Or power users can download 3rd party components
  (perhaps even research-quality components) without affecting other users.

/////////////////////////////////////////////////////////////////////////

I have power users who will want to override my global MCA parameters; is this possible?
----------------------------------------------------------------------------------------

Absolutely.

:doc:`See the run-time tuning FAQ category </faq/tuning>` for information how to set MCA parameters, both at the
system level and on a per-user (or per-MPI-job) basis.

/////////////////////////////////////////////////////////////////////////

What MCA parameters should I, the system administrator, set?
------------------------------------------------------------

This is a difficult question and depends on both your specific
parallel setup and the applications that typically run there.

The best thing to do is to use the ``ompi_info`` command to see what
parameters are available and relevant to you.  Specifically,
``ompi_info`` can be used to show all the parameters that are available
for each plug-in.  Two common places that system administrators like
to tweak are:

* *Only allow specific networks:* Say you have a cluster with a
  high-speed interconnect (such as InfiniBand) and a
  low-speed ethernet network (e.g., 1Gpbps).  The high-speed network is intended for MPI jobs;
  the control network is intended for NFS and other
  administrative-level tasks.  In this case, you can simply turn off Open
  MPI's TCP support.  The ``btl`` framework contains Open MPI's network
  support; in this case, you want to disable the ``tcp`` plug-in.  You can
  do this by adding the following line in the file
  ``$prefix/etc/openmpi-mca-params.conf``:

  .. code-block::

      btl = ^tcp

  This tells Open MPI to load all BTL components *except* ``tcp``.

  Consider another example: your cluster has two TCP networks, one for
  NFS and administration-level jobs, and another for MPI jobs.  You can
  tell Open MPI to ignore the TCP network used by NFS by adding the
  following line in the file ``$prefix/etc/openmpi-mca-params.conf``:

  .. code-block::

      btl_tcp_if_exclude = lo,eth0

  The value of this parameter is the device names to exclude.  In this
  case, we're excluding ``lo`` (localhost, because Open MPI has its own
  internal loopback device) and ``eth0``.

* *Tune the parameters for specific networks:* Each network plug-in
  has a variety of different tunable parameters.  Use the ``ompi_info``
  command to see what is available.  You show *all* available parameters
  with:

  .. code-block::

      shell$ ompi_info --param all all

  .. note:: Starting with Open MPI v1.8, ``ompi_info`` categorizes its
     parameters in *levels*, as defined by the MPI_T interface from
     the MPI standard.  You will need to specify ``--level 9`` (or
     ``--all``) to show *all* MCA parameters.  `See this blog entry
     <https://blogs.cisco.com/performance/open-mpi-and-the-mpi-3-mpi_t-interface>`_
     for further information.

  .. code-block:: sh

     shell$ ompi_info --level 9
     # or
     shell$ ompi_info --all

  Beware: there are *many* variables available.  You can limit the
  output by showing all the parameters in a specific framework or in a
  specific plug-in with the command line parameters:

  .. code-block:: sh

     shell$ ompi_info --param btl all --level 9

  Shows all the parameters of all BTL components, and:

  .. code-block:: sh

     shell$ ompi_info --param btl tcp --level 9

  Shows all the parameters of just the ``tcp`` BTL component.

/////////////////////////////////////////////////////////////////////////

I just added a new plugin to my Open MPI installation; do I need to recompile all my MPI apps?
----------------------------------------------------------------------------------------------

If your installation of Open MPI uses shared libraries and
components are standalone plug-in files, then no.  If you add a new
component (such as support for a new network), Open MPI will simply
open the new plugin at run-time |mdash| your applications do not need to be
recompiled or re-linked.

/////////////////////////////////////////////////////////////////////////

I just upgraded my InfiniBand network; do I need to recompile all my MPI apps?
------------------------------------------------------------------------------

If your installation of Open MPI uses shared libraries and
components are standalone plug-in files, then no.  You simply need to
recompile the Open MPI components that support that network and
re-install them.

More specifically, Open MPI shifts the dependency on the underlying
network away from the MPI applications and to the Open MPI plug-ins.
This is a major advantage over many other MPI implementations.

MPI applications will simply open the new plugin when they run.

/////////////////////////////////////////////////////////////////////////

We just upgraded our version of Open MPI; do I need to recompile all my MPI apps?
---------------------------------------------------------------------------------

It depends on which version of Open MPI your applications were initially compiled against and the target version of Open MPI to which you upgraded.

:doc:`See the section on Open MPI's version numbering scheme </version-numbering>` for more information.

/////////////////////////////////////////////////////////////////////////

I have an MPI application compiled for another MPI; will it work with Open MPI?
-------------------------------------------------------------------------------

It is strongly unlikely.  Open MPI does not attempt to
interface to other MPI implementations, nor executables that were
compiled for them.  Sorry!

MPI applications need to be compiled and linked with Open MPI in order
to run under Open MPI.

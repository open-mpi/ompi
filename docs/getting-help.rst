Getting help
============

If you have a problem or question, it is highly recommended that you
execute the following steps **in order**.  Many people have similar
problems with configuration and initial setup of Open MPI |mdash| it
is possible that your question has already been answered.

#. :doc:`Validate your Open MPI installation </validate>`.  This
   ensures that you have a nominally-correct Open MPI installation.

#. `Check prior GitHub issues
   <https://github.com/open-mpi/ompi/issues>`_ and see if others have
   asked the same question and had it answered.

#. `Check the mailing list archives
   <https://www.open-mpi.org/community/lists/ompi.php>`_ "search"
   features (or use Google) to check old posts and see if others have
   asked the same question and had it answered.

#. If you do not find a solution to your problem in the above
   resources, proceed to the :ref:`Where to send?
   <getting-help-where-to-send-label>` section.

.. _getting-help-where-to-send-label:

Where to send?
--------------

Different types of questions and problems should be sent to different
places.  If you have:

#. **A general end-user question or problem:** you should probably
   subscribe to the `Open MPI user's mailing list
   <https://www.open-mpi.org/community/lists/ompi.php>`_ and post it
   there.

   .. note:: Because of spam, only subscribers to the mailing list are
      allowed to post to the mailing list.  Specifically: **you must
      subscribe to the mailing list before posting.**

   * If you have a compile-time question or problem, see the :ref:`For
     problems building or installing Open MPI
     <getting-help-compile-time-label>` section below for the content
     of what to include in your email.

   * If you have problems launching your MPI or OpenSHMEM application
     successfully, see the :ref:`For problems launching MPI or
     OpenSHMEM applications <getting-help-launching-label>` section
     below for the content of what to include in your email.

   * If you have other questions or problems about running your MPI or
     OpenSHMEM application, see the :ref:`For problems running MPI or
     OpenSHMEM applications <getting-help-running-label>` section
     below for the content of what to include in your email.

   .. important:: The more information you include in your report, the
      better.  E-mails/bug reports simply stating, "It doesn't work!"
      are not helpful; we need to know as much information about your
      environment as possible in order to provide meaningful
      assistance.

   **The best way to get help** is to provide a "recipe" for
   reproducing the problem.  This will allow the Open MPI developers
   to see the error for themselves, and therefore be able to fix it.

   .. important:: Please **use a descriptive "subject" line in your
      email!** Some Open MPI question-answering people decide whether
      to read a mail based on its subject line (e.g., to see if it's a
      question that they can answer).  So please *please* **please**
      use a good subject line that succinctly describes your problem.

#. **A bug report:** you should probably post it to `Open MPI's Github
   issue tracker <https://github.com/open-mpi/ompi/issues>`_.  Follow
   the template to submit all the requested information.

#. **A patch, bug fix, or other code submission:** please post a Github
   Pull Request to the `Open MPI Github repository
   <https://github.com/open-mpi/ompi/pulls>`_.

#. **A developer-level / internal question about Open MPI itself:** you
   should probably subscribe to the `Open MPI developer's mailing list
   <https://www.open-mpi.org/community/lists/ompi.php>`_ and post it
   there.

If you're unsure where to send your question, subscribe and send an
email to the user's mailing list (i.e., option #1, above).

.. _getting-help-compile-time-label:

For problems building or installing Open MPI
--------------------------------------------

If you cannot successfully configure, build, or install Open MPI,
please provide *all* of the following information:

#. The version of Open MPI that you're using.

#. The stdout and stderr from running ``configure``.

#. All ``config.log`` files from the Open MPI build tree.

#. Output from when you ran ``make V=1 all`` to build Open MPI.

#. Output from when you ran ``make install`` to install Open MPI.

The script below may be helpful to gather much of the above
information (adjust as necessary for your specific environment):

.. code-block:: bash

   #!/usr/bin/env bash

   set -euxo pipefail

   # Make a directory for the output files
   dir="`pwd`/ompi-output"
   mkdir $dir

   # Fill in the options you want to pass to configure here
   options=""
   ./configure $options 2>&1  | tee $dir/config.out
   tar -cf - `find . -name config.log` | tar -x -C $dir -

   # Build and install Open MPI
   make V=1 all 2>&1          | tee $dir/make.out
   make install 2>&1          | tee $dir/make-install.out

   # Bundle up all of these files into a tarball
   filename="ompi-output.tar.bz2"
   tar -jcf $filename `basename $dir`
   echo "Tarball $filename created"

Then attach the resulting ``ompi-output.tar.bz2`` file to your report.

.. caution:: The mailing lists have **a 150 KB size limit on
   messages** (this is a limitation of the mailing list web archives).
   If attaching the tarball makes your message larger than 150 KB, you
   may need to post the tarball elsewhere and include a link to that
   tarball in your mail to the list.

.. _getting-help-launching-label:

For problems launching MPI or OpenSHMEM applications
----------------------------------------------------

If you cannot successfully launch simple applications across multiple
nodes (e.g., the non-MPI ``hostname`` command, or the MPI "hello world"
or "ring" sample applications in the ``examples/`` directory), please
provide *all* of the information from the :ref:`For problems building
or installing Open MPI <getting-help-compile-time-label>` section, and
*all* of the following additional information:

#. The output of the ``ompi_info --all`` command from the node where
   you are invoking :ref:`mpirun(1) <man1-mpirun>`.

#. If you have questions or problems about process mapping or binding,
   send the output from running the ``lstopo -v`` and ``lstopo --of
   xml`` commands from a recent version of `Hwloc
   <https://www.open-mpi.org/projects/hwloc/>`_.

#. If running on more than one node, also include the output of the
   ``ompi_info --version`` command **from each node on which you are
   trying to run**.

#. The output of running ``mpirun --map-by ppr:1:node --prtemca
   plm_base_verbose 100 --prtemca rmaps_base_verbose 100 --display
   alloc hostname``.  Add in a ``--hostfile`` argument if needed for
   your environment.

The script below may be helpful to gather much of the above
information (adjust as necessary for your specific environment).

.. note:: It is safe to run this script after running the script from
   the :ref:`building and installing
   <getting-help-compile-time-label>` section.

.. code-block:: bash

   #!/usr/bin/env bash

   set -euxo pipefail

   # Make a directory for the output files
   dir="`pwd`/ompi-output"
   mkdir -p $dir

   # Get installation and system information
   ompi_info --all 2>&1       | tee $dir/ompi-info-all.out
   lstopo -v                  | tee $dir/lstopo-v.txt
   lstopo --of xml            | tee $dir/lstopo.xml

   # Have a text file "my_hostfile.txt" containing the hostnames on
   # which you are trying to launch
   for host in `cat my_hostfile.txt`; do
       ssh $host ompi_info --version 2>&1 | tee $dir/ompi_info-version-$host.out
       ssh $host lstopo -v                | tee $dir/lstopo-v-$host.txt
       ssh $host lstopo --of xml          | tee $dir/lstopo-$host.xml
   done

   # Have a my_hostfile.txt file if needed for your environment, or
   # remove the --hostfile argument altogether if not needed.
   set +e
   mpirun \
        --hostfile my_hostfile.txt \
        --map-by ppr:1:node \
        --prtemca plm_base_verbose 100 \
        --prtemca rmaps_base_verbose 100 \
        --display alloc \
        hostname 2>&1                     | tee $dir/mpirun-hostname.out

   # Bundle up all of these files into a tarball
   filename="ompi-output.tar.bz2"
   tar -jcf $filename `basename $dir`
   echo "Tarball $filename created"

.. _getting-help-running-label:

For problems running MPI or OpenSHMEM applications
--------------------------------------------------

If you can successfully launch parallel MPI or OpenSHMEM applications,
but the jobs fail during the run, please provide *all* of the
information from the :ref:`For problems building or installing Open
MPI <getting-help-compile-time-label>` section, *all* of the
information from the :ref:`For problems launching MPI or OpenSHMEM
applications <getting-help-launching-label>` section, and then *all*
of the following additional information:

#. A *detailed* description of what is failing.  *The more details
   that you provide, the better.* Please include at least the
   following information:

   * The exact command used to run your application.

   * Any relevant MCA parameters that were set (or unset) when
     you ran (from either the command line, environment,
     parameter file, etc.).

   * The value of the ``PATH`` and ``LD_LIBRARY_PATH``
     environment variables (did you set them correctly to point
     to all relevant executables, the Open MPI libraries, and
     any required support libraries, such as libraries required
     for high-speed networks such as InfiniBand).

#. The source code of a short sample program (preferably in C or
   Fortran) that exhibits the problem.

#. If you are experiencing networking problems, include detailed
   information about your network.

   .. error:: TODO Update link to IB FAQ entry.

   #. For RoCE- or InfiniBand-based networks, include the information
      :ref:`in this FAQ entry <faq-ib-troubleshoot-label>`.

   #. For Ethernet-based networks (including RoCE-based networks),
      include the output of the ``ip addr`` command (or the legacy
      ``ifconfig`` command) on all relevant nodes.

      .. note:: Some Linux distributions do not put ``ip`` or
                ``ifconfig`` in the default ``PATH`` of normal users.
                Try looking for it in ``/sbin`` or ``/usr/sbin``.

Troubleshooting
===============

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

I see strange messages about missing symbols in my application; what do these mean?
-----------------------------------------------------------------------------------

Open MPI loads a lot of plugins (sometimes called "components" or
"modules") at run time.  Sometimes a plugin can fail to load because it
can't resolve all the symbols that it needs.  There are a few reasons
why this can happen.

* The plugin is for a different version of Open MPI.  :ref:`See this
  section <building-open-mpi-install-overwrite-label>` for an
  explanation of how Open MPI might try to open the "wrong" plugins.
* An application is trying to manually dynamically open ``libmpi`` in
  a private symbol space.  For example, if an application is not
  linked against ``libmpi``, but rather calls something like this:

  .. code-block:: c

     /* This is a Linux example -- the issue is similar/the same on other
        operating systems */
     handle = dlopen("libmpi.so", RTLD_NOW | RTLD_LOCAL);

  This is due to some deep run time linker voodoo |mdash| it is
  discussed towards the end of `this post to the Open MPI developer's
  list
  <https://www.mail-archive.com/devel@lists.open-mpi.org/msg07981.html>`_.
  Briefly, the issue is this:

  #. The dynamic library ``libmpi`` is opened in a "local" symbol
     space.
  #. ``MPI_INIT`` is invoked, which tries to open Open MPI's plugins.
  #. Open MPI's plugins rely on symbols in ``libmpi`` (and other Open
     MPI support libraries); these symbols must be resolved when the
     plugin is loaded.
  #. However, since ``libmpi`` was opened in a "local" symbol space,
     its symbols are not available to the plugins that it opens.
  #. Hence, the plugin fails to load because it can't resolve all of
     its symbols, and displays a warning message to that effect.

  The ultimate fix for this issue is a bit bigger than Open MPI,
  unfortunately |mdash| it's a POSIX issue (as briefly described in the
  devel mailing list posting, above).

  However, there are several common workarounds:

  * Dynamically open ``libmpi`` in a public / global symbol scope
    |mdash| not a private / local scope.  This will enable
    ``libmpi``'s symbols to be available for resolution when Open MPI
    dynamically opens its plugins.
  * If ``libmpi`` is opened as part of some underlying framework where
    it is not possible to change the private / local scope to a public
    / global scope, then dynamically open ``libmpi`` in a public /
    global scope before invoking the underlying framework.  This
    sounds a little gross (and it is), but at least the run-time
    linker is smart enough to not load ``libmpi`` twice |mdash| but it
    does keeps ``libmpi`` in a public scope.
  * Use the ``--disable-dlopen`` or ``--disable-mca-dso`` options to
    Open MPI's ``configure`` script (see this TODO NONEXISTANT FAQ entry
    for more details on these
    options).  These options slurp all of Open MPI's plugins up in to
    ``libmpi`` |mdash| meaning that the plugins physically reside in
    ``libmpi`` and will not be dynamically opened at run time.
  * Build Open MPI as a static library by configuring Open MPI with
    ``--disable-shared`` and ``--enable-static``.  This has the same
    effect as ``--disable-dlopen``, but it also makes ``libmpi.a`` (as
    opposed to a shared library).

/////////////////////////////////////////////////////////////////////////

How do I attach a parallel debugger to my MPI job?
--------------------------------------------------

.. error:: TODO Need to update this with PMIx debugger info.

/////////////////////////////////////////////////////////////////////////

How do I find out what MCA parameters are being seen/used by my job?
--------------------------------------------------------------------

MCA parameters are the "life's blood" of Open MPI. MCA parameters are
used to control both detailed and large-scale behavior of Open MPI and
are present throughout the code base.

This raises an important question: since MCA parameters can be set from a
file, the environment, the command line, and even internally within Open MPI,
how do I actually know what MCA params my job is seeing, and their value?

One way, of course, is to use the ``ompi_info`` command, which is
documented elsewhere (you can use ``man ompi_info``, or ``ompi_info
--help`` to get more info on this command). However, this still
doesn't fully answer the question since ``ompi_info`` isn't an MPI
process.

To help relieve this problem, Open MPI provides the MCA parameter
``mpi_show_mca_params`` that directs the ``MPI_COMM_WORLD`` rank 0
process to report the name of MCA parameters, their current value as
seen by that process, and the source that set that value.  The
parameter can take several values that define which MCA parameters to
report:

* ``all``: report all MCA params. Note that this typically generates a
  rather long list of parameters since it includes all of the default
  parameters defined inside Open MPI
* ``default``: MCA params that are at their default settings - i.e.,
  all MCA params that are at the values set as default within Open MPI
* ``file``: MCA params that had their value set by a file
* ``api``: MCA params set using Open MPI's internal APIs, perhaps to
  override an incompatible set of conditions specified by the user
* ``enviro``: MCA params that obtained their value either from the
  local environment or the command line. Open MPI treats environmental
  and command line parameters as equivalent, so there currently is no
  way to separate these two sources

These options can be combined in any order by separating them with
commas.

Here is an example of the output generated by this parameter:

.. code-block:: sh

   shell$ mpirun --mca mpi_show_mca_params enviro hello_c
   [local-hostname:12345] mpi_show_mca_params=enviro (environment)
   Hello, World, I am 0 of 1

Note that several MCA parameters set by Open MPI itself for internal
uses are displayed in addition to the ones actually set by the user.

Since the output from this option can be long, and since it can be
helpful to have a more permanent record of the MCA parameters used for
a job, a companion MCA parameter ``mpi_show_mca_params_file`` is
provided. If ``mpi_show_mca_params_file`` is *also* set, the output
listing of MCA parameters will be directed into the specified file
instead of being printed to stdout.  For example:

.. code-block:: sh

   shell$ mpirun --mca mpi_show_mca_params enviro \
       --mca mpi_show_mca_param_file /tmp/foo.txt hello_c
   Hello, World, I am 0 of 1
   shell$ cat /tmp/foo.txt
   #
   # This file was automatically generated on Sun Feb  7 14:34:31 2021
   # by MPI_COMM_WORLD rank 0 (out of a total of 16) on savbu-usnic-a
   #
   mpi_show_mca_params=enviro (environment)
   mpi_show_mca_params_file=/tmp/foo.txt (environment)

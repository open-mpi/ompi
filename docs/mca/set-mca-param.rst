.. _label-running-setting-mca-param-values:

Setting MCA parameter values
============================

MCA parameters may be set in several different ways.

.. admonition:: Rationale
   :class: tip

   Having multiple methods to set MCA parameters allows, for example,
   system administrators to fine-tune the Open MPI installation for
   their hardware / environment such that normal users can simply use
   the default values (that were set by the system administrators).

   HPC environments |mdash| and the applications that run on them
   |mdash| tend to be unique.  Providing extensive run-time tuning
   capabilities through MCA parameters allows the customization of
   Open MPI to each system's / user's / application's particular
   needs.

The following are the different methods to set MCA parameters, listed
in priority order:

#. Command line parameters
#. Environment variables
#. Tuning MCA parameter files
#. Configuration files

.. danger:: Due to how the PMIx and PRRTE projects both evolved to
            become independent projects from Open MPI (:ref:`see this
            section for more detail
            <label-running-role-of-pmix-and-prte>`), they both have
            their own MCA system for setting MCA parameters.

            Hence, all the information about MCA parameters below
            *also* applies to PMIx and PRRTE.

.. _label-mca-set-param-command-line:

Command line parameters
-----------------------

The highest-precedence method is setting MCA parameters on the command
line.  For example:

.. code-block:: sh

   shell$ mpirun --mca mpi_show_handle_leaks 1 --np 4 a.out

This sets the MCA parameter ``mpi_show_handle_leaks`` to the value of
1 before running ``a.out`` with four processes.  In general, the
format used on the command line is ``--mca <param_name> <value>``.

.. note:: When setting a value that includes spaces, you need to use
          quotes to ensure that the shell understands that the
          multiple tokens are a single value.  For example:

          .. code-block:: sh

             shell$ mpirun --mca param "value with multiple words" ...

.. warning:: Setting Open MPI MCA parameters via the command line
             entails using the ``--mca`` CLI option.  When setting
             PMIx- and PRRTE-specific MCA parameters via configuration
             files, use a different CLI option:

             +----------+----------------+
             | Open MPI | ``--mca``      |
             +----------+----------------+
             | PMIx     | ``--pmixmca``  |
             +----------+----------------+
             | PRRTE    | ``--prtemca``  |
             +----------+----------------+

.. _label-mca-set-param-env-var:

Environment variables
---------------------

Next, environment variables are searched.  Any environment variable
named ``OMPI_MCA_<param_name>`` will be used.  For example, the
following has the same effect as the previous example (for sh-flavored
shells):

.. code-block:: sh

   shell$ export OMPI_MCA_mpi_show_handle_leaks=1
   shell$ mpirun --np 4 a.out

.. note:: Just like with command line values, setting environment
          variables to values with multiple words requires shell
          quoting, such as:

          .. code-block:: sh

             shell$ export OMPI_MCA_param="value with multiple words"

.. warning:: Setting Open MPI MCA parameters via environment variables
             entails prefixing the parameter name with ``OMPI_MCA_``.
             When setting PMIx- and PRRTE-specific MCA parameters via
             environment variables, use a different prefix:

             +----------+----------------+
             | Open MPI | ``OMPI_MCA_``  |
             +----------+----------------+
             | PMIx     | ``PMIX_MCA_``  |
             +----------+----------------+
             | PRRTE    | ``PRTE_MCA_``  |
             +----------+----------------+

.. _label-mca-set-param-tune:

Tuning MCA parameter files
--------------------------

Simple text files can be used to set MCA parameter values for a
specific application.

The ``mpirun --tune`` CLI option allows users to specify both MCA
parameters and environment variables from within a single file.

MCA parameters set in tuned parameter files will override any MCA
parameters supplied in global parameter files (e.g.,
``$HOME/.openmpi/mca-params.conf``), but not command line or
environment parameters.

Consider a tuned parameter file name ``foo.conf`` that is placed in
the same directory as the application ``a.out``. A user will typically
run the application as:

.. code-block:: sh

   shell$ mpirun --np 2 a.out

To use the ``foo.conf`` tuned parameter file, this command line
changes to:

.. code-block:: sh

   shell$ mpirun --np 2 --tune foo.conf a.out

Tuned parameter files can be coupled if more than one file is to be
used. If there is another tuned parameter file called ``bar.conf``, it
can be added to the command line as follows:

.. code-block:: sh

   shell$ mpirun --np 2 --tune foo.conf,bar.conf a.out

The contents of tuned files consist of one or more lines, each of
which contain zero or more `-x` and `--mca` options.  Comments are not
allowed.  For example, the following tuned file:

.. code-block::

   -x envvar1=value1 -mca param1 value1 -x envvar2
   -mca param2 value2
   -x envvar3

is equivalent to:

.. code-block:: sh

   shell$ mpirun \
       -x envvar1=value1 -mca param1 value1 -x envvar2 \
       -mca param2 value2
       -x envvar3 \
       ...rest of mpirun command line...

Although the typical use case for tuned parameter files is to be
specified on the command line, they can also be set as MCA parameters
in the environment.  The MCA parameter ``mca_base_param_file_prefix``
contains a comma-delimited list of tuned parameter files exactly as
they would be passed to the ``--tune`` command line option.  The MCA
parameter ``mca_base_param_file_path`` specifies the path to search
for tuned files with relative paths.

.. note:: Starting with OMPI version 1.9, the ``--am`` option to supply
          aggregate MCA (AMCA) parameter files is deprecated. Users
          should instead use the ``--tune`` option. The usage of
          the ``--tune`` option is the same as that for the ``--am``
          option except that ``--tune`` requires a single file or a
          comma delimited list of files, while a colon delimiter is
          used with the ``--am`` option.

.. _label-mca-set-param-config-file:

Configuration files
-------------------

Finally, simple configuration text files can be used to set MCA
parameter values.  Parameters are set one per line (comments are
permitted).  For example:

.. code-block:: ini

   # This is a comment
   # Set the same MCA parameter as in previous examples
   mpi_show_handle_leaks = 1

Note that quotes are *not* necessary for setting multi-word values
in MCA parameter files.  Indeed, if you use quotes in the MCA
parameter file, they will be used as part of the value itself.  For
example:

.. code-block:: ini

   # The following two values are different:
   param1 = value with multiple words
   param2 = "value with multiple words"

By default, two files are searched (in order):

#. ``$HOME/.openmpi/mca-params.conf``: The user-supplied set of
   values takes the highest precedence.
#. ``$prefix/etc/openmpi-mca-params.conf``: The system-supplied set
   of values has a lower precedence.

.. note:: The MCA parameters ``mca_param_files`` or
          ``mca_base_param_files`` were considered to overlap in
          functionality with the ``--tune`` option. They are now
          deprecated and will no longer be read.

.. note:: These parameter files are read during startup and their
          values are sent to all nodes in the job. This is to avoid
          all nodes pulling copies from the filesystem. This is
          contrary to previous behavior where these files were
          read on each node during each process' startup.

.. warning:: Setting Open MPI MCA parameters via configuration files
             entails editing (by default) the ``mca-params.conf`` or
             ``openmpi-mca-params.conf`` files.  When setting PMIx-
             and PRRTE-specific MCA parameters via configuration
             files, set them (by default) in different files:

             +----------+------------------------------------------+
             | Open MPI | ``$HOME/.openmpi/mca-params.conf`` or    |
             |          | ``$prefix/etc/openmpi-mca-params.conf``  |
             +----------+------------------------------------------+
             | PMIx     | ``$HOME/.pmix/mca-params.conf`` or       |
             |          | ``$prefix/etc/pmix-mca-params.conf``     |
             +----------+------------------------------------------+
             | PRRTE    | ``$HOME/.prte/mca-params.conf`` or       |
             |          | ``$prefix/etc/prte-mca-params.conf``     |
             +----------+------------------------------------------+

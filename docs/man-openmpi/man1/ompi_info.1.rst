.. _man1-ompi_info:


ompi_info
=========

.. include_body

ompi_info |mdash| Display information about the Open MPI installation


SYNOPSIS
--------

``ompi_info [options]``


DESCRIPTION
-----------

``ompi_info`` provides detailed information about the Open MPI
installation. It can be useful for at least three common scenarios:

#. Checking local configuration and seeing how Open MPI was installed.

#. Submitting bug reports / help requests to the Open MPI community
   (see :doc:`Getting help </getting-help>`).

#. Seeing a list of installed Open MPI plugins and querying what MCA
   parameters they support.

.. note:: ``ompi_info`` defaults to only showing a few MCA parameters
          by default (i.e., level 1 parameters). Use the ``--level``
          option to enable showing more options (see the :ref:`LEVELS
          <man1-ompi_info-levels>` section for more information).


OPTIONS
-------

``ompi_info`` accepts the following options:

* ``-a``, ``--all``: Show all configuration options and MCA
  parameters. Also changes the default MCA parameter level to 9,
  unless ``--level`` is also specified.

* ``--arch``: Show architecture on which Open MPI was compiled.

* ``-c``, ``--config``: Show configuration options

* ``-gmca``, ``--gmca <param> <value>``: Pass global MCA parameters
  that are applicable to all contexts.

* ``-h``, ``--help``: Shows help / usage message.

* ``--hostname``: Show the hostname on which Open MPI was configured
  and built.

* ``--internal``: Show internal MCA parameters (not meant to be
  modified by users).

* ``--level <level>``: Show only variables with at most this level
  (1-9). The default is 1 unless ``--all`` is specified without
  ``--level``, in which case the default is 9. See the :ref:`LEVELS
  <man1-ompi_info-levels>` section for more information.

* ``-mca``, ``--mca <param> <value>``: Pass context-specific MCA
  parameters; they are considered global if ``--gmca`` is not used and
  only one context is specified.

* ``--param <type> <component>``: Show MCA parameters. The first
  parameter is the type of the component to display; the second
  parameter is the specific component to display (or the keyword
  ``all``, meaning "display all components of this type").

* ``-t``, ``--type``: Show MCA parameters of the type specified in the
  parameter. Accepts the following parameters: ``unsigned_int``,
  ``unsigned_long``, ``unsigned_long_long``, ``size_t``, ``string``,
  ``version_string``, ``bool``, ``double``. By default level is 1
  unless it is specified with ``--level``.

* ``--parsable``: When used in conjunction with other parameters, the
  output is displayed in a machine-parsable format ``--parseable``
  Synonym for ``--parsable``.

* ``--path <type>``: Show paths that Open MPI was configured
  with. Accepts the following parameters: ``prefix``, ``bindir``,
  ``libdir``, ``incdir``, ``pkglibdir``, ``sysconfdir``.

* ``--pretty``: When used in conjunction with other parameters, the output is
  displayed in "prettyprint" format (default)

* ``--selected-only``: Show only variables from selected components.

* ``-V``, ``--version``: Show version of Open MPI.

.. _man1-ompi_info-levels:

LEVELS
------

Open MPI has many, many run-time tunable parameters (called "MCA
parameters"), and usually only a handful of them are useful to a given
user.

As such, Open MPI has divided these parameters up into nine distinct
levels, broken down into three categories, each with three
sub-categories.

Note that since each MCA parameter is accessible through the MPI_T
control variable API (introduced in MPI-3.0), these levels exactly
correspond to the nine MPI_T cvar levels.

The three categories are:

#. **End user**: Generally, these are parameters that are required for
   correctness, meaning that a user may need to set these just to get
   their MPI application to run correctly. For example, BTL
   ``if_include`` and ``if_exclude`` parameters fit into this
   category.

#. **Application tuner**: Generally, these are parameters that can be
   used to tweak MPI application performance. This even includes
   parameters that control resource exhaustion levels (e.g., number of
   free list entries, size of buffers, etc.), and could be considered
   "correctness" parameters if they're set too low. But, really --
   they're tuning parameters.

#. **Open MPI developer**: Parameters in this category either don't
   fit in the other two, or are specifically intended for debugging /
   development of Open MPI itself.

And within each category, there are three sub-categories:

#. **Basic**: This sub-category is for parameters that everyone in
   this category will want to see -- even less-advanced end users,
   application tuners, and new OMPI developers.

#. **Detailed**: This sub-category is for parameters that are
   generally useful, but users probably won't need to change them
   often.

#. **All**: This sub-category is for all other parameters. Such
   parameters are likely fairly esoteric.

Combining the categories and sub-categories, here's how Open MPI
defines all nine levels:

#. Basic information of interest to end users.
#. Detailed information of interest to end users.
#. All remaining information of interest to end users.
#. Basic information required for application tuners.
#. Detailed information required for application tuners.
#. All remaining information required for application tuners.
#. Basic information for Open MPI implementors.
#. Detailed information for Open MPI implementors.
#. All remaining information for Open MPI implementors.

By default, ``ompi_info`` only shows level 1 MCA parameters. To see
more MCA parameters, use the ``--level`` command line option.


EXAMPLES
--------

Show the default output of options and listing of installed
components in a human-readable / prettyprint format:

.. code-block::

   ompi_info

Show the default output of options and listing of installed components
in a machine-parsable format:

.. code-block::

   ompi_info --parsable

Show the level 1 MCA parameters of the "tcp" BTL component in a
human-readable / prettyprint format:

.. code-block::

   ompi_info --param btl tcp

Show the level 1 through level 6 MCA parameters of the "tcp" BTL
component in a human-readable / prettyprint format:

.. code-block::

   ompi_info --param btl tcp --level 6

Show the level 1 MCA parameters of the "tcp" BTL component in a
machine-parsable format:

.. code-block::

   ompi_info --param btl tcp --parsable

Show the level 1 through level 3 MCA parameters of string type in a
human-readable / prettyprint format:

.. code-block::

   ompi_info --type string --pretty-print --level 3

Show the "bindir" that Open MPI was configured with:

.. code-block::

   ompi_info --path bindir

Show the version of Open MPI version numbers in a prettyprint format:

.. code-block::

   ompi_info --version

Show *all* information about the Open MPI installation, including all
components that can be found, all the MCA parameters that they support
(i.e., levels 1 through 9), versions of Open MPI and the components,
etc.:

.. code-block::

   ompi_info --all

.. _oshmem_info:


oshmem_info
===========

.. include_body

oshmem_info - Display information about the OpenSHMEM installation


SYNOPSIS
--------

``oshmem_info [options]``


DESCRIPTION
-----------

``oshmem_info`` provides detailed information about the OpenSHMEM
installation. It can be useful for at least three common scenarios:

1. Checking local configuration and seeing how OpenSHMEM was
   installed.

2. Submitting bug reports / help requests to the OpenSHMEM
   community
   (see :doc:`Getting help </getting-help>`).

3. Seeing a list of installed OpenSHMEM plugins and querying what MCA
   parameters they support.

.. note:: ``oshmem_info`` defaults to only showing a few MCA parameters
          by default (i.e., level 1 parameters). Use the ``--level``
          option to enable showing more options (see the :ref:`LEVELS
          <man1-oshmem_info-levels>` section for more information).


OPTIONS
-------

``oshmem_info`` accepts the following options:

* ``-a``, ``--all``: Show all configuration options and MCA parameters

* ``--arch``: Show architecture OpenSHMEM was compiled on

* ``-c``, ``--config``: Show configuration options

* ``-gmca``, ``--gmca <param> <value>*``: Pass global MCA parameters
  that are applicable to all contexts.

* ``-h``, ``--help*``: Shows help / usage message

* ``--hostname``: Show the hostname that OpenSHMEM was configured
  and built on

* ``--internal``: Show internal MCA parameters (not meant to be
  modified by users)

* ``--level``: Show only variables with at most this level (1-9). The
  default is 1 unless ``--all`` is specified without ``--level`` in
  which case the default is 9. See the :ref:`LEVELS
  <man1-oshmem_info-levels>` section for more information.

* ``-mca``, ``--mca <param> <value>``: Pass context-specific MCA
  parameters; they are considered global if
  ``--gmca`` is not used and only one context is specified.

* ``--param <type> <component>``: Show MCA parameters. The first
  parameter is the type of the component to display; the second
  parameter is the specific component to display (or the keyword
  ``all``, meaning "display all components of this type").

* ``--parsable``: When used in conjunction with other parameters,
  the output is displayed in a machine-parsable format ``--parseable``
  Synonym for ``--parsable``.

* ``--path <type>``: Show paths that OpenSHMEM was configured
  with. Accepts the following parameters: ``prefix``, ``bindir``,
  ``libdir``, ``incdir``, ``pkglibdir``, ``sysconfdir``.

* ``--pretty``: When used in conjunction with other parameters, the
  output is displayed in prettyprint format (default)

* ``--selected-only``: Show only variables from selected components.

* ``-V``, ``--version*``: Show version of OpenSHMEM.

.. _man1-oshmem_info-levels:

LEVELS
------

#. Basic information of interest to users
#. Detailed information of interest to users
#. All remaining information of interest to users
#. Basic information required for tuning
#. Detailed information required for tuning
#. All remaining information required for tuning
#. Basic information for OpenSHMEM implementors
#. Detailed information for OpenSHMEM implementors
#. All remaining information for OpenSHMEM implementors


EXAMPLES
--------

.. code:: sh

   shell$ oshmem_info

Show the default output of options and listing of installed components
in a human-readable / prettyprint format.

.. code:: sh

   shell$ oshmem_info --parsable

Show the default output of options and listing of installed components
in a machine-parsable format.

.. code:: sh

   shell$ oshmem_info --param btl openib

Show the MCA parameters of the "openib" BTL component in a
human-readable / prettyprint format.

.. code:: sh

   shell$ oshmem_info --param btl openib --parsable

Show the MCA parameters of the "openib" BTL component in a
machine-parsable format.

.. code:: sh

   shell$ oshmem_info --path bindir

Show the "bindir" that OpenSHMEM was configured with.

.. code:: sh

   shell$ oshmem_info --version ompi full --parsable

Show the full version numbers of OpenSHMEM (including the ORTE
and OPAL version numbers) in a machine-readable format.

.. code:: sh

   shell$ oshmem_info --version btl major

Show the major version number of all BTL components in a prettyprint
format.

.. code:: sh

   shell$ oshmem_info --version btl:tcp minor

Show the minor version number of the TCP BTL component in a
prettyprint format.

.. code:: sh

   shell$ oshmem_info --all

Show *all* information about the OpenSHMEM installation, including all
components that can be found, the MCA parameters that they support,
versions of OpenSHMEM and the components, etc.

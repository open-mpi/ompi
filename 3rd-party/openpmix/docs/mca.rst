.. _label-mca:

The Modular Component Architecture (MCA)
========================================

PMIx is a highly-customizable system; it can be configured via
configuration files, command line parameters, and environment
variables.  The main functionality of PMIx's configuration system
is through the Modular Component Architecture (MCA).

* This section describes the MCA itself and how to set MCA parameters at
  run time.
* Later sections in this documentation describe different parts of
  PMIx's functionality, and the specific names and values of MCA
  parameters that can be used to affect PMIx's behavior.

/////////////////////////////////////////////////////////////////////////

Terminology
-----------

The Modular Component Architecture (MCA) is the backbone for much of
PMIx's functionality.  It is a series of *frameworks*,
*components*, and *modules* that are assembled at run-time to create
the PMIx implementation.

MCA *parameters* (also known as MCA *variables*) are used to customize
PMIx's behavior at run-time.

Each of these entities are described below.

Frameworks
^^^^^^^^^^

An MCA framework manages zero or more components at run-time and is
targeted at a specific task (e.g., providing support for network
functionality).  Although each MCA framework supports only a single
type of component, it may support multiple components of that type.

Some of the more common frameworks that users may want or need to
customize include the following:

* ``ptl``: PMIx Transport Layer; users may need to assist the
  messaging layer with selection of network interfaces and
  controls parameters
* ``pmdl``: Programming Model and Library; identify environmental
  variables to be forwarded to application processes

There are many frameworks within PMIx; the exact set varies
between different versions of PMIx.  You can use the
:ref:`pmix_info(1) <man1-pmix_info>` command to see the full list of
frameworks that are included in PMIx |opmix_ver|.

Components
^^^^^^^^^^

An MCA component is an implementation of a framework's formal
interface.  It is a standalone collection of code that can be bundled
into a plugin that can be inserted into the PMIx code base, either
at run-time and/or compile-time.

.. note:: Good synonyms for PMIx's "component" concept are
          "plugin", or "add-on".

The exact set of components varies between different versions of Open
MPI.  PMIx's code base includes support for many components, but
not all of them may be present or available on your system.  You can
use the :ref:`pmix_info(1) <man1-pmix_info>` command to see what
components are included in PMIx |opmix_ver| on your system.

Modules
^^^^^^^

An MCA module (frequently called a ``plugin``) is an instance of a
component.  While it is possible for a component to have multiple
active plugins, it would be a very rare component that did so.

Parameters (variables)
^^^^^^^^^^^^^^^^^^^^^^

MCA *parameters* (sometimes called MCA *variables*) are the basic unit
of run-time tuning for PMIx.  They are simple "key = value" pairs
that are used extensively throughout PMIx.  The general rules of
thumb that the developers use are:

#. Instead of using a constant for an important value, make it an MCA
   parameter so users can adjust it if necessary.
#. If a task can be implemented in multiple, user-discernible ways,
   implement as many as possible, and use an MCA parameter to
   choose between them at run-time.

For example, the PMIx server sets a maximum value on the number of
events it will cache. Events received after that point cause the
oldest events in the cache to be released. This allows the server
to provide a process with events it registers to receive, even if
the registration occurs after the event has been received by the
server - thereby reducing the likelihood of a process missing an
event due to a race condition. Applications that generate a large
volume of events (e.g., an application relying on the PMIx Group
invite/join method for creating collections of processes) might
require a significantly larger cache than the default size.

/////////////////////////////////////////////////////////////////////////

.. _label-running-setting-mca-param-values:

Setting MCA parameter values
----------------------------

MCA parameters may be set in several different ways.

.. admonition:: Rationale
   :class: tip

   Having multiple methods to set MCA parameters allows, for example,
   system administrators to fine-tune the PMIx installation for
   their hardware / environment such that normal users can simply use
   the default values (that were set by the system administrators).

   HPC environments |mdash| and the applications that run on them
   |mdash| tend to be unique.  Providing extensive run-time tuning
   capabilities through MCA parameters allows the customization of
   PMIx to each system's / user's / application's particular
   needs.

The following are the different methods to set MCA parameters, listed
in priority order:

#. Environment variables
#. Configuration files


Environment variables
^^^^^^^^^^^^^^^^^^^^^

Environment variables are given the highest priority.  Any environment variable
named ``PMIX_MCA_<param_name>`` will be examined for use. Note that
misspelling of names will cause the value to be ignored - i.e.,
PMIx does not report unrecognized MCA parameters, it simply ignores
them.

.. note:: Just like with command line values, setting environment
          variables to values with multiple words requires shell
          quoting, such as:

          .. code-block:: sh

             shell$ export OMPI_MCA_param="value with multiple words"


Configuration files
^^^^^^^^^^^^^^^^^^^

Simple configuration text files can also be used to set MCA
parameter values.  Parameters are set one per line (comments are
permitted).  For example:

.. code-block:: ini

   # This is a comment
   # Set an MCA parameter
   mca_component_show_load_errors = 1

Note that quotes are *not* necessary for setting multi-word values
in MCA parameter files.  Indeed, if you use quotes in the MCA
parameter file, they will be used as part of the value itself.  For
example:

.. code-block:: ini

   # The following two values are different:
   param1 = value with multiple words
   param2 = "value with multiple words"

By default, two files are searched (in order):

#. ``$HOME/.pmix/mca-params.conf``: The user-supplied set of
   values has the higher precedence.
#. ``$prefix/etc/pmix-mca-params.conf``: The system-supplied set
   of values has a lower precedence.

More specifically, the MCA parameter ``mca_param_files`` specifies a
colon-delimited path of files to search for MCA parameters.  Files to
the left have lower precedence; files to the right are higher
precedence.

.. note:: Keep in mind that, just like components, these parameter
          files are *only* relevant where they are "visible". Typically,
          these files are read by the host daemon responsible for
          launching an application and then forwarded to all
          daemons (and their child application processes) in their
          environment.

.. warning:: Setting PMIx MCA parameters via configuration files
             entails editing (by default) the following files:

             ``$HOME/.pmix/mca-params.conf`` or
             ``$prefix/etc/pmix-mca-params.conf``

/////////////////////////////////////////////////////////////////////////

.. _label-running-selecting-framework-components:

Selecting which PMIx components are used at run time
--------------------------------------------------------

Each MCA framework has a top-level MCA parameter that helps guide
which components are selected to be used at run-time.  Specifically,
every framework has an MCA parameter of the same name that can be used
to *include* or *exclude* components from a given run.

For example, the ``pmdl`` MCA parameter can used to control which PMDL
components are used.  It takes a comma-delimited list of component
names, and may be optionally prefixed with ``^``.  For example:

.. note:: The Programming Model ``PMDL`` framework provides support
  for a range of programming models and libraries, including collection
  of default parameters and environmental variables for forwarding and
  setting of library-specific environmental variables

.. code-block:: sh

   # Tell PMIx to include *only* the PMDL components listed here and
   # implicitly ignore all the rest:
   export PMIX_MCA_pmdl=ompi,oshmem ...

   # Tell PMIx to exclude the ompi and oshmem PMDL components
   # and implicitly include all the rest
   export PMIX_MCA_pmdl=^ompi,oshmem ...

Note that ``^`` can *only* be the prefix of the *entire*
comma-delimited list because the inclusive and exclusive behavior are
mutually exclusive.  Specifically, since the exclusive behavior means
"use all components *except* these", it does not make sense to mix it
with the inclusive behavior of not specifying it (i.e., "use all of
these components").  Hence, something like this:

.. code-block:: sh

   export PMIX_MCA_pmdl=ompi,^oshmem ...

does not make sense |mdash| and will cause an error |mdash| because it
says "use only the ``ompi`` component" but
also "use all components except ``oshmem``".  These two statements
clearly contradict each other.

/////////////////////////////////////////////////////////////////////////

Common MCA parameters
---------------------

PMIx has a *large* number of MCA parameters available.  Users can
use the :ref:`pmix_info(1) <man1-pmix_info>` command to see *all*
available MCA parameters.

The vast majority of these MCA parameters, however, are not useful to
most users.  Although the full list of MCA parameters can be found in the output of
``pmix_info(1)``, the following list of commonly-used parameters is
presented here so that they can easily be found via internet searches:

* Individual framework names with the ``_base_verbose`` suffix
  appended (e.g., ``ptl_base_verbose``, ``pmdl_base_verbose``, etc.)
  can be used to set the general verbosity level of all the components
  in that framework.

  * This can be helpful when troubleshooting why certain components
    are or are not being selected at run time.

* The PMIx Transport Layer supports "include" and "exclude"
  types of components (e.g., ``ptl_tcp_if_include`` and
  ``ptl_tcp_if_exclude``).  The "include" parameters specify an
  explicit set of network interfaces to use; the "exclude" parameters
  specify an explicit set of network interfaces to ignore.  Check the
  output from :ref:`pmix_info(1)'s <man1-pmix_info>` to see the full list
  of PTL-related parameters.

  .. important:: You can only use the "include" *or* the "exclude"
                 parameter |mdash| they are mutually exclusive from each
                 other.
* ``mca_base_component_show_load_errors``: By default, PMIx
  emits a warning message if it fails to open a DSO component at run
  time.  This typically happens when a shared library that the DSO
  requires is not available.

  .. admonition:: Rationale
     :class: tip

     In prior versions of PMIx, components defaulted to building
     as DSOs (vs. being included in the parent library,
     ``libpmix.so``).  On misconfigured systems, sometimes libraries
     required by various components would not be present, thereby causing
     those components to fail to open at run time.

     Having PMIx warn about such failures to load was useful
     because it alerted users to the misconfiguration.

  .. note:: By default, PMIx |opmix_ver| includes all components in
            its base libraries (e.g., on Linux, ``libpmix.so`` includes
            all the components that were built with PMIx, and
            therefore no component need to be opened dynamically), and
            does not build its components as DSOs.

            This MCA parameter *only* affects the behavior when a
            component DSO fails to open.

  This MCA parameter can take four general values:

  #. ``yes`` or a boolean "true" value (e.g., ``1``): PMIx will
     emit a warning about every component DSO that fails to load.

  #. ``no`` or a boolean "false" value (e.g., ``0``): PMIx will
     never emit warnings about component DSOs that fail to load.

  #. A comma-delimited list of frameworks and/or components: PMIx
     will emit a warning about any dynamic component that fails to
     open and matches a token in the list. "Match" is defined as:

     * If a token in the list is only a framework name, then any
       component in that framework will match.
     * If a token in the list specifies both a framework name and a
       component name (in the form ``framework/component``), then
       only the specified component in the specified framework will
       match.

     For example, if the value of this MCA parameter is
     ``pmdl,pnet/opa``, then PMIx will warn if any component in
     the PMDL framework or if the OPA PNET component fails to load at run
     time.

  #. The value can also be a ``^`` character followed by a
     comma-delimited list of ``framework[/component]`` values: This
     is similar to the comma-delimited list of tokens, except it will
     only emit warnings about dynamic components that fail to load
     and do *not* match a token in the list.

     For example, if the value of this MCA parameter is
     ``^pmdl,pnet/opa``, then PMIx will only warn about the
     failure to load DSOs that are neither in the PMDL
     framework nor are the OPA PNET component.

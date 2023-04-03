Common MCA parameters
=====================

Open MPI has a *large* number of MCA parameters available.  Users can
use the :ref:`ompi_info(1) <man1-ompi_info>` command to see *all*
available MCA parameters.

.. note:: Similarly, you can use the ``pmix_info(1)`` and
          ``prte_info(1)`` commands to see all the MCA parameters
          available for the PMIx and PRRTE projects, respectively.

          The documentation for these commands are not included in the
          Open MPI docs, but they are both quite similar to
          :ref:`ompi_info(1) <man1-ompi_info>`.

The vast majority of these MCA parameters, however, are not useful to
most users.  Indeed, there only are a handful of MCA parameters that
are commonly used by end users.  :ref:`As described in the
ompi_info(1) man page <man1-ompi_info-levels>`, MCA parameters are
grouped into nine levels, corresponding to the MPI standard's tool
support verbosity levels.  In general:

* Levels 1-3 are intended for the end user.

  * These parameters are generally used to effect whether an Open MPI
    job will be able to run correctly.

  .. tip:: Parameters in levels 1-3 are probably applicable to
           most end users.

* Levels 4-6 are intended for the application tuner.

  * These parameters are generally used to tune the performance of an
    Open MPI job.

* Levels 7-9 are intended for the MPI implementer.

  * These parameters are esoteric and really only intended for those
    who work deep within the implementation of Open MPI code base
    itself.

Although the full list of MCA parameters can be found in the output of
``ompi_info(1)``, the following list of commonly-used parameters is
presented here so that they can easily be found via internet searches:

* Individual framework names are used as MCA parameters to
  :ref:`select which components will be used
  <label-running-selecting-framework-components>`.  For example, the
  ``btl`` MCA parameter is used to select which components will be
  used from the ``btl`` framework.  The ``coll`` MCA parameter is used
  to select which ``coll`` components are used.  And so on.

* Individual framework names with the ``_base_verbose`` suffix
  appended (e.g., ``btl_base_verbose``, ``coll_base_verbose``, etc.)
  can be used to set the general verbosity level of all the components
  in that framework.

  * This can be helpful when troubleshooting why certain components
    are or are not being selected at run time.

* Many network-related components support "include" and "exclude"
  types of components (e.g., ``btl_tcp_if_include`` and
  ``btl_tcp_if_exclude``).  The "include" parameters specify an
  explicit set of network interfaces to use; the "exclude" parameters
  specify an explicit set of network interfaces to ignore.  Check the
  output from :ref:`ompi_info(1)'s <man1-ompi_info>` full list to see
  if the network-related component you are using has "include" and
  "exclude" network interface parameters.

  .. important:: You can only use the "include" *or* the "exclude"
                 parameter |mdash| they are mutually exclusive from each                                                
                 other.
* ``opal_mca_base_component_show_load_errors``: By default, Open MPI
  emits a warning message if it fails to open a DSO component at run
  time.  This typically happens when a shared library that the DSO
  requires is not available.

  .. admonition:: Rationale
     :class: tip

     In prior versions of Open MPI, components defaulted to building
     as DSOs (vs. being included in their parent libraries, such as
     ``libmpi.so``).  On misconfigured systems, sometimes network
     acceleration libraries would not be present, meaning that
     HPC-class networking components failed to open at run time.  As
     such, Open MPI would typically fall back to TCP as a network
     transport, which usually led to poor performance of end-user
     applications.

     Having Open MPI warn about such failures to load was useful
     because it alerted users to the misconfiguration.

  .. note:: By default, Open MPI |ompi_ver| includes all components in
            its base libraries (e.g., on Linux, ``libmpi.so`` includes
            all the components that were built with Open MPI, and
            therefore no component need to be opened dynamically), and
            does not build its components as DSOs.

            This MCA parameter *only* affects the behavior of when a
            component DSO fails to open.

  This MCA parameter can take four general values:

  #. ``yes`` or a boolean "true" value (e.g., ``1``): Open MPI will
     emit a warning about every component DSO that fails to load.

  #. ``no`` or a boolean "false" value (e.g., ``0``): Open MPI will
     never emit warnings about component DSOs that fail to load.

  #. A comma-delimited list of frameworks and/or components: Open MPI
     will emit a warning about any dynamic component that fails to
     open and matches a token in the list. "Match" is defined as:

     * If a token in the list is only a framework name, then any
       component in that framework will match.
     * If a token in the list specifies both a framework name and a
       component name (in the form ``framework/component``), then
       only the specified component in the specified framework will
       match.

     For example, if the value of this MCA parameter is
     ``accelerator,btl/uct``, then Open MPI warn if any component in
     the accelerator framework or if the UCT BTL fails to load at run
     time.

  #. The value can also be a ``^`` character followed by a
     comma-delimited list of ``framework[/component]`` values: This
     is similar to the comma-delimited list of tokens, except it will
     only emit warnings about dynamic components that fail to load
     and do *not* match a token in the list.

     For example, if the value of this MCA parameter is
     ``^accelerator,btl/uct``, then Open MPI will only warn about the
     failure to load DSOs that are neither in the accelerator
     framework nor are the UCT BTL.

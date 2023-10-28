.. _label-building-installation-cli-options:

Installation options
^^^^^^^^^^^^^^^^^^^^

The following are general installation command line options that can
be used with ``configure``:

* ``--prefix=DIR``:
  Install Open MPI into the base directory named ``DIR``.  Hence, Open
  MPI will place its executables in ``DIR/bin``, its header files in
  ``DIR/include``, its libraries in ``DIR/lib``, etc.

  .. note:: Also see the section on :ref:`installation location
            <building-open-mpi-installation-location-label>` for more
            information on the installation prefix.

* ``--disable-shared``: By default, Open MPI and OpenSHMEM build
  shared libraries, and all components are included as part of those
  shared libraries. This switch disables this default; it is really
  only useful when used with ``--enable-static``.  Specifically, this
  option does *not* imply ``--enable-static``; enabling static
  libraries and disabling shared libraries are two independent
  options.

  .. tip::

     :ref:`See this section <label-install-packagers-dso-or-not>` for
     advice to packagers about this CLI option.

* ``--enable-static``:
  Build MPI and OpenSHMEM as static libraries, and statically link in
  all components.  Note that this option does *not* imply
  ``--disable-shared``; enabling static libraries and disabling shared
  libraries are two independent options.

  Be sure to read the description of ``--without-memory-manager``,
  below; it may have some effect on ``--enable-static``.

  .. tip::

     :ref:`See this section <label-install-packagers-dso-or-not>` for
     advice to packagers about this CLI option.

* ``--disable-wrapper-runpath`` / ``--disable-wrapper-rpath``: By
  default, the wrapper compilers (e.g., ``mpicc``) will explicitly add
  "runpath" and "rpath" linker flags when linking user executables on
  systems that support them.  That is, the created executables will
  include a filesystem path reference to the location of Open MPI's
  libraries in the application executable itself.  This means that the
  user does not have to set ``LD_LIBRARY_PATH`` to find Open MPI's
  libraries, which can be helpful if they are installed in a location
  that the run-time linker does not search by default.

  Using the ``--disable-wrapper-r*path`` options will prevent the
  wrappers from explicitly adding one or both of these linker flags.

  .. note:: By default, the wrapper compilers prefer "runpath"
            behavior over "rpath" behavior.

            * Using ``--disable-wrapper-runpath`` alters this
              preference: explicit "runpath" linker flags will not be
              added by the wrappers.  However, "rpath" flags may still
              be added, if the platform supports them.
            * Using both ``--disable-wrapper-runpath`` *and*
              ``--disable-wrapper-rpath`` will prevent the wrappers
              from explicitly adding "runpath" *and* "rpath" linker
              flags.

  .. caution:: Even if the wrapper compilers do not explicitly add
               "runpath" or "rpath" linker flags, the local compiler,
               linker, and/or operating system may implicitly enable
               either "runpath" or "rpath" behavior when linking.

  .. important:: The ``--disable-wrapper-runpath`` and
                 ``--disable-wrapper-rpath`` CLI options *only* affect
                 the flags that the wrapper compilers use when
                 building MPI/SHMEM applications.  These options do
                 not affect how Open MPI or OpenSHMEM are built (to
                 include the wrapper compilers themselves).

                 See the :ref:`Linker "rpath" and "runpath"
                 functionality
                 <building-ompi-cli-options-rpath-and-runpath-label>`
                 section for details on how "rpath" and "runpath"
                 affect the building and linking of Open MPI itself.

  When either of "runpath" or "rpath" behaviors are enabled, the
  applications will have the filesystem path location of the Open MPI
  libraries hard-coded into the Open MPI/OpenSHMEM application.  The
  most notable differences between "runpath" and "rpath" behavior are:

  * runpath

     #. The run-time linker first searches the paths in the
        ``LD_LIBRARY_PATH`` environment variable for the relevant Open
        MPI/OpenSHMEM libraries.
     #. If not found there, the run-time linker falls back to checking
        the hard-coded location for the relevant Open MPI/OpenSHMEM
        libraries.

  * rpath

     #. The run-time linker first checks the hard-coded location for
        the relevant Open MPI/OpenSHMEM libraries.
     #. If not found there, the run-time linker falls back to
        searching the paths in the ``LD_LIBRARY_PATH`` environment
        variable for the relevant Open MPI/OpenSHMEM libraries.

  .. warning:: There are other, subtle differences between "runpath"
               and "rpath" which are out of scope for this
               documentation.  You may wish to consult other sources
               for more information.

               For example, a decent set of explanations can be found
               in the slides for a Linux course entitled "Building and
               Using Shared Libraries on Linux // `Shared Libraries:
               The Dynamic Linker
               <https://man7.org/training/download/shlib_dynlinker_slides.pdf>`_".

  For example, consider that you install Open MPI vA.B.0 and
  compile/link your MPI/OpenSHMEM application against it.  Later, you
  install Open MPI vA.B.1 to a different installation prefix (e.g.,
  ``/opt/openmpi/A.B.1`` vs. ``/opt/openmpi/A.B.0``), and you leave
  the old installation intact.

  In the runpath case, you can set the ``LD_LIBRARY_PATH`` environment
  variable to point to the A.B.1 installation, and then your MPI
  application will use those libraries, since the runtime will search
  the paths in ``LD_LIBRARY_PATH`` first.

  In the rpath case, since the run-time linker searches the
  ``/opt/openmpi/A.B.0`` location that is hard-coded in your MPI
  application first, your application will use the libraries from your
  A.B.0 installation (regardless of the value of the
  ``LD_LIBRARY_PATH`` environment variable).

  Note that in both cases, however, if you remove the original A.B.0
  installation and set ``LD_LIBRARY_PATH`` to point to the A.B.1
  installation, your application will use the A.B.1 libraries.

  As noted above, both runpath/rpath behaviors can be disabled via
  ``--disable-wrapper-rpath``.

  .. note:: You can also :ref:`customize the compiler/linker flags
            that are used by the wrapper compilers
            <label-customizing-wrapper-compiler>` to build Open
            MPI/OpenSHMEM applications.

.. _building-ompi-cli-options-diable-dlopen-label:

* ``--enable-dlopen``: Enable Open MPI to load components as
  standalone Dynamic Shared Objects (DSOs) at run-time.  This option
  is enabled by default.

  The opposite of this option, ``--disable-dlopen``, causes the following:

  #. Open MPI will not attempt to open any DSOs at run-time.
  #. configure behaves as if the ``--enable-mca-static`` argument was set.
  #. configure will ignore the ``--enable-mca-dso`` argument.

  See the description of ``--enable-mca-static`` / ``--enable-mca-dso`` for
  more information.

  .. note:: This option does *not* change how Open MPI's libraries
            (``libmpi``, for example) will be built.  You can change
            whether Open MPI builds static or dynamic libraries via
            the ``--enable|disable-static`` and
            ``--enable|disable-shared`` arguments.

.. _building-ompi-cli-options-mca-dso-label:

* ``--enable-mca-dso[=LIST]`` and ``--enable-mca-static[=LIST]``
  These two options, along with ``--enable-mca-no-build``, govern the
  behavior of how Open MPI's frameworks and components are built.

  The ``--enable-mca-dso`` option specifies which frameworks and/or
  components are built as Dynamic Shared Objects (DSOs).
  Specifically, DSOs are built as "plugins" outside of the core Open
  MPI libraries, and are loaded by Open MPI at run time.

  The ``--enable-mca-static`` option specifies which frameworks and/or
  components are built as part of the core Open MPI libraries (i.e.,
  they are not built as DSOs, and therefore do not need to be
  separately discovered and opened at run time).

  Both options can be used one of two ways:

  #. ``--enable-mca-OPTION`` (with no value)
  #. ``--enable-mca-OPTION=LIST``

  ``--enable-mca-OPTION=no`` or ``--disable-mca-OPTION`` are both legal
  options, but have no impact on the selection logic described below.
  Only affirmative options change the selection process.

  If ``LIST`` is not specified (e.g., ``--enable-mca-dso`` with no
  ``LIST``), or if ``LIST`` is the special value ``yes``, then *all*
  components will be selected.  If ``LIST`` is specified, it is a
  comma-delimited list of Open MPI frameworks and/or
  framework+component tuples.  Examples:

  * ``btl`` specifies the entire BTL framework
  * ``btl-tcp`` specifies just the TCP component in the BTL framework
  * ``mtl,btl-tcp`` specifies the entire MTL framework and the TCP
    component in the BTL framework

  Open MPI's ``configure`` script uses the values of these two options
  when evaluating each component to determine how it should be built
  by evaluating these conditions in order:

  #. If an individual component's build behavior has been specified
     via these two options, ``configure`` uses that behavior.
  #. Otherwise, if the component is in a framework whose build
     behavior has been specified via these two options, ``configure``
     uses that behavior.
  #. Otherwise, ``configure`` uses the global default build behavior.

  At each level of the selection process, if the component is
  specified to be built as both a static and DSO component, the static
  option will win.

  .. note:: As of Open MPI |ompi_ver|, ``configure``'s global default
            is to build all components as static (i.e., part of the
            Open MPI core libraries, not as DSOs).  Prior to Open MPI
            5.0.0, the global default behavior was to build
            most components as DSOs.

  .. important:: If the ``--disable-dlopen`` option is specified, then
                 Open MPI will not be able to search for DSOs at run
                 time, and the value of the ``--enable-mca-dso``
                 option will be silently ignored.

  Some examples:

  #. Default to building all components as static (i.e., as part of
     the Open MPI core libraries -- no DSOs)::

        shell$ ./configure

  #. Build all components as DSOs::

        shell$ ./configure --enable-mca-dso

  #. Build all components as static, except the TCP BTL, which will be
     built as a DSO::

        shell$ ./configure --enable-mca-dso=btl-tcp

  #. Build all components as static, except all BTL components, which
     will be built as DSOs::

        shell$ ./configure --enable-mca-dso=btl

  #. Build all components as static, except all MTL components and the
     TCP BTL component, which will be built as DSOs::

        shell$ ./configure --enable-mca-dso=mtl,btl-tcp

  #. Build all BTLs as static, except the TCP BTL, as the
     ``<framework-component>`` option is more specific than the
     ``<framework>`` option::

        shell$ ./configure --enable-mca-dso=btl --enable-mca-static=btl-tcp

  #. Build the TCP BTL as static, because the static option at the
     same level always wins::

        shell$ ./configure --enable-mca-dso=btl-tcp --enable-mca-static=btl-tcp

  .. tip::

     :ref:`See this section <label-install-packagers-dso-or-not>` for
     advice to packagers about this CLI option.

* ``--enable-mca-no-build=LIST``: Comma-separated list of
  ``<framework>-<component>`` pairs that will not be built. For
  example, ``--enable-mca-no-build=threads-qthreads,pml-monitoring`` will
  disable building both the ``qthreads`` threading component and the
  ``monitoring`` PML.

  .. note:: This option is typically only useful for components that
            would otherwise be built.  For example, if you are on a
            machine without Libfabric support, it is not necessary to
            specify::

              shell$ ./configure --enable-mca-no-build=cm-ofi

            because the ``configure`` script will naturally see that
            you do not have support for Libfabric and will
            automatically skip the ``ofi`` CM component.

* ``--disable-show-load-errors-by-default``:
  Set the default value of the ``mca_base_component_show_load_errors``
  MCA variable: the ``--enable`` form of this option sets the MCA
  variable to true, the ``--disable`` form sets the MCA variable to
  false.  The MCA ``mca_base_component_show_load_errors`` variable can
  still be overridden at run time via the usual MCA-variable-setting
  mechanisms; this configure option simply sets the default value.

  The ``--disable`` form of this option is intended for Open MPI
  packagers who tend to enable support for many different types of
  networks and systems in their packages.  For example, consider a
  packager who includes support for both the FOO and BAR networks in
  their Open MPI package, both of which require support libraries
  (``libFOO.so`` and ``libBAR.so``).  If an end user only has BAR
  hardware, they likely only have ``libBAR.so`` available on their
  systems -- not ``libFOO.so``.  Disabling load errors by default will
  prevent the user from seeing potentially confusing warnings about
  the FOO components failing to load because ``libFOO.so`` is not
  available on their systems.

  Conversely, system administrators tend to build an Open MPI that is
  targeted at their specific environment, and contains few (if any)
  components that are not needed.  In such cases, they might want
  their users to be warned that the FOO network components failed to
  load (e.g., if ``libFOO.so`` was mistakenly unavailable), because Open
  MPI may otherwise silently failover to a slower network path for MPI
  traffic.

* ``--with-platform=FILE``:
  Load configure options for the build from ``FILE``.  Options on the
  command line that are not in ``FILE`` are also used.  Options on the
  command line and in ``FILE`` are replaced by what is in ``FILE``.

* ``--with-libmpi-name=STRING``:
  Replace ``libmpi.*`` and ``libmpi_FOO.*`` (where ``FOO`` is one of the
  fortran supporting libraries installed in lib) with ``libSTRING.*``
  and ``libSTRING_FOO.*``. This is provided as a convenience mechanism
  for third-party packagers of Open MPI that might want to rename
  these libraries for their own purposes. This option is *not*
  intended for typical users of Open MPI.

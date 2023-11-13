General notes
=============

The following list of release notes applies to this code base as of
this writing:

* Open MPI now includes two public software layers: MPI and OpenSHMEM.
  Throughout this document, references to Open MPI implicitly include
  both of these layers. When distinction between these two layers is
  necessary, we will reference them as the "MPI" and "OpenSHMEM"
  layers respectively.

* OpenSHMEM is a collaborative effort between academia, industry, and
  the U.S. Government to create a specification for a standardized API
  for parallel programming in the Partitioned Global Address Space
  (PGAS).  For more information about the OpenSHMEM project, including
  access to the current OpenSHMEM specification, please visit
  http://openshmem.org/.

  .. note:: This OpenSHMEM implementation will only work in Linux
            environments with a restricted set of supported networks.

* Open MPI includes support for a wide variety of supplemental
  hardware and software packages.  When configuring Open MPI, you may
  need to supply additional flags to the ``configure`` script in order
  to tell Open MPI where the header files, libraries, and any other
  required files are located.  As such, running ``configure`` by itself
  may not include support for all the devices (etc.) that you expect,
  especially if their support headers / libraries are installed in
  non-standard locations.  Network interconnects are an easy example
  to discuss |mdash| Libfabric and OpenFabrics networks, for example, both
  have supplemental headers and libraries that must be found before
  Open MPI can build support for them.  You must specify where these
  files are with the appropriate options to configure.  See the
  listing of configure command-line switches, below, for more details.

* The majority of Open MPI's documentation is here in this document.
  The man pages are also installed by default.

* Note that Open MPI documentation uses the word "component"
  frequently; the word "plugin" is probably more familiar to most
  users.  As such, end users can probably completely substitute the
  word "plugin" wherever you see "component" in our documentation.
  For what it's worth, we use the word "component" for historical
  reasons, mainly because it is part of our acronyms and internal API
  function calls.

* Open MPI has taken some steps towards `Reproducible Builds
  <https://reproducible-builds.org/>`_.  Specifically, Open MPI's
  ``configure`` and ``make`` process, by default, records the build date
  and some system-specific information such as the hostname where Open
  MPI was built and the username who built it.  If you desire a
  Reproducible Build, set the ``$SOURCE_DATE_EPOCH``, ``$USER`` and
  ``$HOSTNAME`` environment variables before invoking ``configure`` and
  ``make``, and Open MPI will use those values instead of invoking
  ``whoami`` and/or ``hostname``, respectively.  See
  https://reproducible-builds.org/docs/source-date-epoch/ for
  information on the expected format and content of the
  ``$SOURCE_DATE_EPOCH`` variable.

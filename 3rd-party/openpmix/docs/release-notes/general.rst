General notes
=============

The following list of release notes applies to this code base as of
this writing:

* PMIx includes support for a wide variety of supplemental
  hardware and software packages.  When configuring PMIx, you may
  need to supply additional flags to the ``configure`` script in order
  to tell PMIx where the header files, libraries, and any other
  required files are located.  As such, running ``configure`` by itself
  may not include support for all the devices (etc.) that you expect,
  especially if their support headers / libraries are installed in
  non-standard locations. See the
  listing of configure command-line switches, below, for more details.

* The majority of PMIx's documentation is now on the `ReadTheDocs site <http://openpmix.readthedocs.io/>`_.
  The man pages are also installed by default.

* Note that PMIx documentation uses the word "component"
  frequently; the word "plugin" is probably more familiar to most
  users.  As such, end users can probably completely substitute the
  word "plugin" wherever you see "component" in our documentation.
  For what it's worth, we use the word "component" for historical
  reasons, mainly because it is part of our acronyms and internal API
  function calls.

* PMIx has taken some steps towards `Reproducible Builds
  <https://reproducible-builds.org/>`_.  Specifically, PMIx's
  ``configure`` and ``make`` process, by default, records the build date
  and some system-specific information such as the hostname where PMIx
  was built and the username who built it.  If you desire a
  Reproducible Build, set the ``$SOURCE_DATE_EPOCH``, ``$USER`` and
  ``$HOSTNAME`` environment variables before invoking ``configure`` and
  ``make``, and PMIx will use those values instead of invoking
  ``whoami`` and/or ``hostname``, respectively.  See
  https://reproducible-builds.org/docs/source-date-epoch/ for
  information on the expected format and content of the
  ``$SOURCE_DATE_EPOCH`` variable.

Release Notes
=============

- Systems that have been tested are:

  - Linux (various flavors/distros), 32 bit, with gcc
  - Linux (various flavors/distros), 64 bit (x86), with gcc, Intel,
    and Portland (*)
  - OS X (10.7 and above), 32 and 64 bit (x86_64), with gcc (*)

- PRRTE has taken some steps towards Reproducible Builds
  (https://reproducible-builds.org/).  Specifically, PRRTE's
  ``configure`` and ``make`` process, by default, records the build
  date and some system-specific information such as the hostname where
  PRRTE was built and the username who built it.  If you desire a
  Reproducible Build, set the ``$SOURCE_DATE_EPOCH``, ``$USER`` and
  ``$HOSTNAME`` environment variables before invoking ``configure``
  and ``make``, and PRRTE will use those values instead of invoking
  ``whoami`` and/or ``hostname``, respectively.  See
  https://reproducible-builds.org/docs/source-date-epoch/ for
  information on the expected format and content of the
  ``$SOURCE_DATE_EPOCH`` variable.

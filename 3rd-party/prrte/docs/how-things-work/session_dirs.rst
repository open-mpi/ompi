.. _session-dir-detail-label:

Session Directories
===================

In general, servers, tools, and application processes all have access to their own ``session directory`` - a location where scratch files can be safely placed with a reasonable guarantee of automatic cleanup upon termination. Session directories provide a safe location (i.e., in a temporary file system and guaranteed not to conflict with other sessions/jobs/applications) for executables to use when creating scratch files such as shared memory backing files and rendezvous files. PMIx and PRRTE also provide a reasonable guarantee that any files and/or subdirectories created under the specified location will be automatically cleaned up at finalize and/or termination. In this case, ``reasonable`` means that we will do our best to remove all files and subdirectories, but cannot fully guarantee removal in situations outside of our control (e.g., being forcibly terminated via `SIGKILL`).

   .. note:: In general, the host (e.g., PRRTE) is responsible for creating session
   		 	 directories. In some cases, PMIx creates a limited set of session
   		 	 directories if the host does not provide them - e.g., in the case of a
   		 	 self-launched tool - for storing contact information. This is outlined
   		 	 below.

The following attributes can be used to pass session directory information to the PMIx library:

* ``PMIX_SYSTEM_TMPDIR``: temporary directory for this system (typically ``/tmp`` for Linux systems).
  The PMIx server will place tool rendezvous points and contact info in this location. In the
  absence of this attribute during ``PMIx_Init`` (or the server/tool version), the PMIx library
  will first look for the ``TMPDIR`` envar, then ``TEMP``, and finally ``TMP`` - if none of
  those are found, the library will default to the ``/tmp`` location.

* ``PMIX_SERVER_TMPDIR``: session directory where the PMIx server will place client rendezvous
  points and contact info. If not provided, the library will first look for the ``PMIX_SERVER_TMPDIR``
  envar, then ``TMPDIR``, ``TEMP``, and finally ``TMP`` - if none of those are found, the
  library will default to the ``/tmp`` location.

* ``PMIX_TMPDIR``: top-level temporary directory assigned to a session. Often equated to
  ``PMIX_SERVER_TMPDIR`` by host environments.

* ``PMIX_NSDIR``: session directory assigned to a namespace. Usually placed underneath the
  ``PMIX_SERVER_TMPDIR`` and given a name based on the namespace itself.

* ``PMIX_PROCDIR``: session directory assigned to an individual process. Usually placed underneath
  the ``PMIX_NSDIR`` assigned to the namespace of the process, and given a string name equivalent
  to the rank of the process.

* ``PMIX_LAUNCHER_RENDEZVOUS_FILE``: the full path name of a file wherein a tool shall output its
  connection information - e.g., a launcher to store contact information so that a debugger
  can attach to it.

* ``PMIX_TDIR_RMCLEAN``: the host environment (often known as the "resource manager" or "RM") will
  cleanup the session directories. In the absence of this attribute, the PMIx library will
  remove all files in any session directory it created and then remove the directory itself.

These same attributes can be used in calls to ``PMIx_Get`` by application processes to retrieve
the session directory locations for their own use.


Client Session Directories
--------------------------

The PMIx client library does not create its own session directories as it does not publish
contact information.  Host daemons often create one or more session directory levels for use
by client application processes (e.g., for storing shared memory backing files) - the location
of those directories is passed to clients using the above attributes.

As the client library never creates session directories, it does not perform any cleanup of
the session directory tree.


Tool and Server Session Directories
-----------------------------------

The PMIx library utilizes appropriate session directory locations to store one or more
"rendezvous files" - i.e., files containing connection information. Note that the library does not
always create all levels of the session directory tree, although the process
itself can of course create directories as it sees fit. Only the directories that (a) are required
for generating the particular rendezvous file, and (b) do not already exist are created. Only
directories actually created by the library are cleaned up and removed upon finalization.

The following rendezvous files are provided:

* if ``PMIX_LAUNCHER_RENDEZVOUS_FILE`` is given (either via an attribute to an "init" function
  or as an envar), then the specified file (including any required path elements) will be created.

* if the server is designated as a "system" server (i.e., the ``PMIX_SERVER_SYSTEM_SUPPORT`` attribute
  was provided to ``PMIx_server_init``), then a rendezvous file named "pmix.sys.<hostname>" will be
  created in the ``PMIX_SYSTEM_TMPDIR`` location.

* if the server is designated as a "session" server (i.e., the ``PMIX_SERVER_SESSION_SUPPORT`` attribute
  was provided to ``PMIx_server_init``), then a rendezvous file named "pmix.sys.<hostname>" will be
  created in the ``PMIX_SERVER_TMPDIR`` location.

* if the server declares that it will support tool connections (i.e., the ``PMIX_SERVER_TOOL_SUPPORT``
  attribute was provided to ``PMIx_server_init``), then the following rendezvous files will be created
  under the ``PMIX_SERVER_TMPDIR`` location:

    * a PID file: "pmix.<hostname>.<pid>"

    * a namespace file using the nspace of the server: "pmix.<hostname>.<nspace>"

* if the server is designated as a "scheduler" (i.e., the ``PMIX_SERVER_SCHEDULER`` attribute
  was provided to ``PMIx_server_init``), then a rendezvous file named "pmix.sched.<hostname>" will be
  created in the ``PMIX_SYSTEM_TMPDIR`` location.

* if the server is designated as a "system controller" (i.e., the ``PMIX_SERVER_SYS_CONTROLLER`` attribute
  was provided to ``PMIx_server_init``), then a rendezvous file named "pmix.sysctrlr.<hostname>" will be
  created in the ``PMIX_SYSTEM_TMPDIR`` location.

   .. note:: The above rendezvous files are additive - i.e., generating any one of the files has
             no bearing on whether another file will be output. Thus, a single server could
             generate anywhere from one to five (or more) rendezvous files spanning several
             directory levels.

   .. warning:: There is a potential conflict in rendezvous file names - e.g., if multiple processes
                declare themselves to be a "session" server on the same node. The format of the
                names used by PMIx are intended to support tool connection in the absence of specific
                connection directives - i.e., they provide a means by which PMIx can search for and
                find the rendezvous file for a particular type of process without requiring the user
                to manually identify it. Thus, a tool can request connection to the "system controller"
                without necessarily knowing the PID of that process and PMIx can facilitate the
                connection. As a result, only one system server can be operating on a node at a time.
                This is also (independently) true for schedulers and system controllers.



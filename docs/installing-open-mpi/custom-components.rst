.. _installing-custom-components-label:

Installing custom components
============================

By default, Open MPI looks in two places for components at
run-time (in order):

#. ``$prefix/lib/openmpi/``: This is the system-provided components
   directory, part of the installation tree of Open MPI itself.
#. ``$HOME/.openmpi/components/``: This is where users can drop their
   own components that will automatically be "seen" by Open MPI at
   run-time.  This is ideal for developmental, private, or otherwise
   unstable components.

Note that the directories and search ordering used for finding
components in Open MPI is, itself, an MCA parameter.  Setting the
``mca_component_path`` changes this value (a colon-delimited list of
directories).

Note also that components are only used on nodes where they are
"visible". Hence, if your ``$prefix/lib/openmpi/`` is a directory on a
local disk that is not shared via a network filesystem to other nodes
where you run MPI jobs, then components that are installed to that
directory will *only* be used by MPI jobs running on the local node.

More specifically: components have the same visibility as normal
files.  If you need a component to be available to all nodes where you
run MPI jobs, then you need to ensure that it is visible on all nodes
(typically either by installing it on all nodes for non-networked
filesystem installs, or by installing them in a directory that is
visible to all nodes via a networked filesystem).  Open MPI does not
automatically send components to remote nodes when MPI jobs are run.

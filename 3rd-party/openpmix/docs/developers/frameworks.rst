.. _label-frameworks:

Internal frameworks
===================

The Modular Component Architecture (MCA) is the backbone of PMIx
-- most services and functionality are implemented through MCA
components.

Here is a list of all the component frameworks in PMIx as of
this writing |date|:

* ``bfrops``: Buffer Operations, including support for pack/unpack,
  copy, print, compare, and load of data types and structured objects
* ``gds``: Generalized DataStore for storing job-level and other data
* ``pcompress``: Compress to support compression of large data objects
* ``pdl``: DLopen support
* ``pfexec``: Fork/Exec support to allow tools to start child processes
* ``pgpu``: GPU support
* ``pif``: Interface discovery
* ``pinstalldirs``: Install Directories - provides a struct containing
  all installation locations
* ``plog``: Logging of user-provided alerts
* ``pmdl``: Programming Model - provides support for a range of
  programming models and libraries, including collection of default
  parameters and environmental variables for forwarding and setting
  of library-specific environmental variables
* ``pnet``: Network support, including computation of endpoints to
  support the ``instant on`` launch procedure
* ``preg``: Regular expression generator and parser
* ``prm``: Resource Manager support - translation of generic PMIx directives
  (e.g., mapping and resource definitions) to RM-specific values and
  general RM-specific support
* ``psec``: Security operations such as connection handshakes
* ``psensor``: Sensor framework for monitoring processes, including
  resource utilization and state-of-health (e.g., heartbeat)
* ``psquash``: Internal framework for squashing integer data values
  during transmission
* ``pstat``: Statistics, including reporting resource usage at the
  process, node, and disk levels
* ``pstrg``: Storage system support for querying availability and
  characteristics of file systems
* ``ptl``: Transport Layer for client-server and tool-server
  communication

Framework notes
---------------

Each framework typically has one or more components that are used at
run-time.  For example, the ``bfrops`` framework is used by PMIx
to pack/unpack data for transmission, copy data objects, and other
data manipulation operations.  The ``v3`` component, for example,
supports the data object definitions introduced in v3 of the
library, while the ``v41`` component supports those introduced
in v4.1.

MCA parameter notes
-------------------

Each component typically has some tunable parameters that can be
changed at run-time.  Use the :ref:`pmix_info(1) <man1-pmix_info>`
command to check a component to see what its tunable parameters are.
For example:

.. code-block:: sh

   shell$ pmix_info --param psensor file

shows the parameters (and default values) for the ``file`` ``psensor``
component.

See :ref:`this section <label-running-setting-mca-param-values>` for
details on how to set MCA parameters at run time.

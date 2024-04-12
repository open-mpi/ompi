Exceptions to the PMIx Standard
===============================

Exceptions to the base PMIx Standard are listed here. These exceptions
are not indicative of any intent to stray
from the Standard, but instead represent the difference between the
pace of development of the library versus the normal Standard's
process. Accordingly, it is expected that the exceptions listed below
will make their way into a future release of the PMIx Standard and
then be removed from the list of exceptions in some future OpenPMIx
release.

Extensions
----------

OpenPMIx |opmix_ver| is based on the PMIx |std_ver| Standard. In
addition to *Extensions* to the Standard, this release includes the conversion of
all support macros to PMIx function APIs |mdash| e.g., the
``PMIX_LOAD_PROCID`` macro is now the ``PMIx_Load_procid()``
function |mdash| in accordance with planned changes to the Standard.
The macro versions have been retained as deprecated (without
warnings) for backward compatibility.

Qualified Values
----------------

OpenPMIx has introduced the concept of ``qualified values`` to allow users to specify a value combined with one or more qualifiers.


Scheduler Integration APIs
--------------------------

* Allow a scheduler to direct the resource manager to execute a session-related operation, or allow the resource manager to report a session-related action (e.g., session terminated) to the scheduler:

  .. code-block:: c

     pmix_status_t PMIx_Session_control(uint32_t sessionID,
                                        const pmix_info_t *directives, size_t ndirs,
                                        pmix_info_cbfunc_t cbfunc, void *cbdata);


Tool APIs
---------

* Check if the tool is connected to a PMIx server:

  .. code-block:: c

     bool PMIx_tool_is_connected(void);

* Allow the tool to register a server function pointer module so it can service client requests:

  .. code-block:: c

     pmix_status_t PMIx_tool_set_server_module(pmix_server_module_t *mod);


Utility APIs
------------

* Load a key:

  .. code-block:: c

     void PMIx_Load_key(pmix_key_t key, const char *src);

* Check a key:

  .. code-block:: c

     bool PMIx_Check_key(const char *key, const char *str);

* Check to see if a key is a "reserved" key:

  .. code-block:: c

     bool PMIx_Check_reserved_key(const char *key);

* Load a string into a ``pmix_nspace_t`` struct:

  .. code-block:: c

     void PMIx_Load_nspace(pmix_nspace_t nspace, const char *str);

* Check two ``nspace`` structs for equality:

  .. code-block:: c

     bool PMIx_Check_nspace(const char *key1, const char *key2);

* Check if a namespace is invalid:

  .. code-block:: c

     bool PMIx_Nspace_invalid(const char *nspace);

* Load a process ID struct:

  .. code-block:: c

     void PMIx_Load_procid(pmix_proc_t *p,
                           const char *ns,
                           pmix_rank_t rk);

* Transfer a process ID struct (non-destructive):

  .. code-block:: c

     void PMIx_Xfer_procid(pmix_proc_t *dst,
                           const pmix_proc_t *src);

* Check two proc IDs for equality:

  .. code-block:: c

     bool PMIx_Check_procid(const pmix_proc_t *a,
                            const pmix_proc_t *b);

* Check two ranks for equality:

  .. code-block:: c

     bool PMIx_Check_rank(pmix_rank_t a,
                          pmix_rank_t b);

* Check if proc ID is invalid:

  .. code-block:: c

     bool PMIx_Procid_invalid(const pmix_proc_t *p);


Argv Handling
-------------
Functions for handling of argv arrays (``NULL``-terminated array of strings)

* Count the number of entries

.. code-block:: c

    int PMIx_Argv_count(char **a);

* Append a string to the array

.. code-block:: c

    pmix_status_t PMIx_Argv_append_nosize(char ***argv, const char *arg);

* Prepend a string to the array

.. code-block:: c

    pmix_status_t PMIx_Argv_prepend_nosize(char ***argv, const char *arg);

* Append a string to the array, but only if it doesn't already
  appear on the array (ignore if it does)

.. code-block:: c

    pmix_status_t PMIx_Argv_append_unique_nosize(char ***argv, const char *arg);

* Free an array, including each string on the array

.. code-block:: c

    void PMIx_Argv_free(char **argv);

* Split a string into an argv array, dividing the string on each occurrence
  of the specified delimiter character. Retain empty entries in the array
  when more than one copy of the delimiter character appears in a sequence.

.. code-block:: c

     char **PMIx_Argv_split_inter(const char *src_string,
                                  int delimiter,
                                  bool include_empty);

* Split a string into an argv array, dividing the string on each occurrence
  of the specified delimiter character. Retain empty entries in the array
  when more than one copy of the delimiter character appears in a sequence.
  Acts as a wrapper to ``PMIx_Argv_split_inter`` with ``include_empty`` set
  to ``true``

.. code-block:: c

    char **PMIx_Argv_split_with_empty(const char *src_string, int delimiter);

* Split a string into an argv array, dividing the string on each occurrence
  of the specified delimiter character. Discard empty entries in the array
  when more than one copy of the delimiter character appears in a sequence.
  Acts as a wrapper to ``PMIx_Argv_split_inter`` with ``include_empty`` set
  to ``false``

.. code-block:: c

    char **PMIx_Argv_split(const char *src_string, int delimiter);

* Join all the elements of an argv array into a single newly-allocated string,
  with the specified delimiter character at the join points.

.. code-block:: c

    char *PMIx_Argv_join(char **argv, int delimiter);

* Copy a ``NULL``-terminated argv array.

.. code-block:: c

    char **PMIx_Argv_copy(char **argv);

* Set environment variable:

  .. code-block:: c

     pmix_status_t PMIx_Setenv(const char *name,
                               const char *value,
                               bool overwrite,
                               char ***env);

Value Struct Functions
----------------------
* Initialize a value struct:

  .. code-block:: c

     void PMIx_Value_construct(pmix_value_t *val);

* Free memory stored inside a value struct:

  .. code-block:: c

     void PMIx_Value_destruct(pmix_value_t *val);

* Create and initialize an array of value structs:

  .. code-block:: c

     pmix_value_t* PMIx_Value_create(size_t n);

* Free memory stored inside an array of coord structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Value_free(pmix_value_t *v, size_t n);

* Check the given value struct to determine if it includes a boolean
  value (includes strings for ``true`` and ``false``, including
  abbreviations such as ``t`` or ``f``), and if so, then its value. A
  value type of ``PMIX_UNDEF`` is taken to imply a boolean ``true``.

  .. code-block:: c

     pmix_boolean_t PMIx_Value_true(const pmix_value_t *v);

* Compare the contents of two ``pmix_value_t`` structures:

  .. code-block:: c

     pmix_value_cmp_t PMIx_Value_compare(pmix_value_t *v1,
                                         pmix_value_t *v2);

* Get the size of the contents of a ``pmix_value_t`` structure:

  .. code-block:: c

     pmix_status_t PMIx_Value_get_size(const pmix_value_t *val,
                                       size_t *size);

Data Array Functions
--------------------
* Construct a data array object, allocating the memory for the indicated
  number of the specified data type. Memory for the provided data array
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Data_array_construct(pmix_data_array_t *p,
                                    size_t num, pmix_data_type_t type);

* Initialize the fields of a data array object without allocating any
  memory for the included array:

  .. code-block:: c

     void PMIx_Data_array_init(pmix_data_array_t *p,
                               pmix_data_type_t type);

* Destroy a data array object, releasing all memory included in it:

  .. code-block:: c

     void PMIx_Data_array_destruct(pmix_data_array_t *d);

* Create and initialize a ``pmix_data_array_t`` structure, allocating the
  memory for the indicated number of the specified data type as well as
  the ``pmix_data_array_t`` object itself:

  .. code-block:: c

     pmix_data_array_t* PMIx_Data_array_create(size_t n, pmix_data_type_t type);

* Free memory stored inside a ``pmix_data_array_t`` structure (does not free
  the provided ``pmix_data_array_t`` object itself):

  .. code-block:: c

     void PMIx_Data_array_free(pmix_data_array_t *p);

Info Struct Functions
---------------------
* Initialize an info struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Info_construct(pmix_info_t *p);

* Free memory stored inside an info struct:

  .. code-block:: c

     void PMIx_Info_destruct(pmix_info_t *p);

* Create and initialize an array of info structs:

  .. code-block:: c

     pmix_info_t* PMIx_Info_create(size_t n);

* Free memory stored inside an array of coord structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Info_free(pmix_info_t *p, size_t n);

* Check the given info struct to determine if it includes
  a boolean value (includes strings for ``true`` and ``false``,
  including abbreviations such as ``t`` or ``f``), and if so,
  then its value. A value type of ``PMIX_UNDEF`` is taken to imply
  a boolean ``true`` as the presence of the key defaults to
  indicating ``true``.

  .. code-block:: c

     pmix_boolean_t PMIx_Info_true(const pmix_info_t *p);

* Mark the info struct as required:

  .. code-block:: c

     void PMIx_Info_required(pmix_info_t *p);

* Mark the info struct as optional:

  .. code-block:: c

     void PMIx_Info_optional(pmix_info_t *p);

* Check if the info struct is required:

  .. code-block:: c

     bool PMIx_Info_is_required(const pmix_info_t *p);

* Check if the info struct is optional:

  .. code-block:: c

     bool PMIx_Info_is_optional(const pmix_info_t *p);

* Mark the info struct as processed:

  .. code-block:: c

     void PMIx_Info_processed(pmix_info_t *p);

* Check if the info struct has been processed:

  .. code-block:: c

     bool PMIx_Info_was_processed(const pmix_info_t *p);

* Mark the info struct as the end of an array:

  .. code-block:: c

     void PMIx_Info_set_end(pmix_info_t *p);

* Check if the info struct is the end of an array:

  .. code-block:: c

     bool PMIx_Info_is_end(const pmix_info_t *p);

* Mark the info as a qualifier:

  .. code-block:: c

     void PMIx_Info_qualifier(pmix_info_t *p);

* Check if the info struct is a qualifier:

  .. code-block:: c

     bool PMIx_Info_is_qualifier(const pmix_info_t *p);

* Mark the info struct as persistent |mdash| do *not* release its contents:

  .. code-block:: c

     void PMIx_Info_persistent(pmix_info_t *p);

* Check if the info struct is persistent:

  .. code-block:: c

     bool PMIx_Info_is_persistent(const pmix_info_t *p);

* Get the size of a ``pmix_info_t`` structure:

  .. code-block:: c

      pmix_status_t PMIx_Info_get_size(const pmix_info_t *val,
                                       size_t *size);

Coordinate Struct Functions
---------------------------
* Initialize a coord struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Coord_construct(pmix_coord_t *m);

* Free memory stored inside a coord struct:

  .. code-block:: c

     void PMIx_Coord_destruct(pmix_coord_t *m);

* Create and initialize an array of coord structs:

  .. code-block:: c

     pmix_coord_t* PMIx_Coord_create(size_t dims,
                                     size_t number);

* Free memory stored inside an array of coord structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Coord_free(pmix_coord_t *m, size_t number);

Topology Functions
------------------
* Initialize a topology struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Topology_construct(pmix_topology_t *t);

* Create and initialize an array of topology structs:

  .. code-block:: c

     pmix_topology_t* PMIx_Topology_create(size_t n);

* Free memory stored inside an array of topology structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Topology_free(pmix_topology_t *t, size_t n);

Cpuset Functions
----------------
* Initialize a cpuset struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Cpuset_construct(pmix_cpuset_t *cpuset);

* Free memory stored inside a cpuset struct:

  .. code-block:: c

     void PMIx_Cpuset_destruct(pmix_cpuset_t *cpuset);

* Create and initialize an array of cpuset structs:

  .. code-block:: c

     pmix_cpuset_t* PMIx_Cpuset_create(size_t n);

* Free memory stored inside an array of cpuset structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Cpuset_free(pmix_cpuset_t *c, size_t n);

Geometry Functions
------------------
* Initialize a geometry struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Geometry_construct(pmix_geometry_t *g);

* Free memory stored inside a cpuset struct:

  .. code-block:: c

     void PMIx_Geometry_destruct(pmix_geometry_t *g);

* Create and initialize an array of cpuset structs:

  .. code-block:: c

     pmix_geometry_t* PMIx_Geometry_create(size_t n);

* Free memory stored inside an array of cpuset structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Geometry_free(pmix_geometry_t *g, size_t n);

Device Distance Functions
-------------------------
* Initialize a device distance struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Device_distance_construct(pmix_device_distance_t *d);

* Free memory stored inside a device distance struct:

  .. code-block:: c

     void PMIx_Device_distance_destruct(pmix_device_distance_t *d);

* Create and initialize an array of device distance structs:

  .. code-block:: c

     pmix_device_distance_t* PMIx_Device_distance_create(size_t n);

* Free memory stored inside an array of device distance structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Device_distance_free(pmix_device_distance_t *d, size_t n);

Byte Object Functions
---------------------
* Initialize a byte object struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Byte_object_construct(pmix_byte_object_t *b);

* Free memory stored inside a byte object struct:

  .. code-block:: c

     void PMIx_Byte_object_destruct(pmix_byte_object_t *g);

* Create and initialize an array of byte object structs:

  .. code-block:: c

     pmix_byte_object_t* PMIx_Byte_object_create(size_t n);

* Free memory stored inside an array of byte object structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Byte_object_free(pmix_byte_object_t *g, size_t n);

* Load a byte object:

  .. code-block:: c

     void PMIx_Byte_object_load(pmix_byte_object_t *b,
                                char *d, size_t sz);

Endpoint Functions
------------------
* Initialize an endpoint struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Endpoint_construct(pmix_endpoint_t *e);

* Free memory stored inside an endpoint struct:

  .. code-block:: c

     void PMIx_Endpoint_destruct(pmix_endpoint_t *e);

* Create and initialize an array of endpoint structs:

  .. code-block:: c

     pmix_endpoint_t* PMIx_Endpoint_create(size_t n);

* Free memory stored inside an array of endpoint structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Endpoint_free(pmix_endpoint_t *e, size_t n);

Envar Functions
---------------
* Initialize an envar struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Envar_construct(pmix_envar_t *e);

* Free memory stored inside an envar struct:

  .. code-block:: c

     void PMIx_Envar_destruct(pmix_envar_t *e);

* Create and initialize an array of envar structs:

  .. code-block:: c

     pmix_envar_t* PMIx_Envar_create(size_t n);

* Free memory stored inside an array of envar structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Envar_free(pmix_envar_t *e, size_t n);

* Load an envar struct:

  .. code-block:: c

     void PMIx_Envar_load(pmix_envar_t *e,
                          char *var,
                          char *value,
                          char separator);

Data Buffer Functions
---------------------
* Initialize a data buffer struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Data_buffer_construct(pmix_data_buffer_t *b);

* Free memory stored inside a data buffer struct:

  .. code-block:: c

     void PMIx_Data_buffer_destruct(pmix_data_buffer_t *b);

* Create a data buffer struct:

  .. code-block:: c

     pmix_data_buffer_t* PMIx_Data_buffer_create(void);

* Free memory stored inside a data buffer struct:

  .. code-block:: c

     void PMIx_Data_buffer_release(pmix_data_buffer_t *b);

* Load a data buffer struct:

  .. code-block:: c

     void PMIx_Data_buffer_load(pmix_data_buffer_t *b,
                                char *bytes, size_t sz);

* Unload a data buffer struct:

  .. code-block:: c

     void PMIx_Data_buffer_unload(pmix_data_buffer_t *b,
                                  char **bytes, size_t *sz);

Proc Struct Functions
---------------------
* Initialize a proc struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Proc_construct(pmix_proc_t *p);

* Clear memory inside a proc struct:

  .. code-block:: c

     void PMIx_Proc_destruct(pmix_proc_t *p);

* Create and initialize an array of proc structs:

  .. code-block:: c

     pmix_proc_t* PMIx_Proc_create(size_t n);

* Free memory stored inside an array of proc structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Proc_free(pmix_proc_t *p, size_t n);

* Load a proc struct:

  .. code-block:: c

     void PMIx_Proc_load(pmix_proc_t *p,
                         char *nspace, pmix_rank_t rank);

* Construct a multicluster ``nspace`` struct from cluster and
  ``nspace`` values:

  .. code-block:: c

     void PMIx_Multicluster_nspace_construct(pmix_nspace_t target,
                                             pmix_nspace_t cluster,
                                             pmix_nspace_t nspace);

* Parse a multicluster nspace struct to separate out the cluster
  and ``nspace`` portions:

  .. code-block:: c

     void PMIx_Multicluster_nspace_parse(pmix_nspace_t target,
                                         pmix_nspace_t cluster,
                                         pmix_nspace_t nspace);

Proc Info Functions
-------------------
* Initialize a proc info struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Proc_info_construct(pmix_proc_info_t *p);

* Clear memory inside a proc info struct:

  .. code-block:: c

     void PMIx_Proc_info_destruct(pmix_proc_info_t *p);

* Create and initialize an array of proc info structs:

  .. code-block:: c

     pmix_proc_info_t* PMIx_Proc_info_create(size_t n);

* Free memory stored inside an array of proc info structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Proc_info_free(pmix_proc_info_t *p, size_t n);

Proc Stats Functions
--------------------
* Initialize a proc stats struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Proc_stats_construct(pmix_proc_stats_t *p);

* Clear memory inside a proc stats struct:

  .. code-block:: c

     void PMIx_Proc_stats_destruct(pmix_proc_stats_t *p);

* Create and initialize an array of proc stats structs:

  .. code-block:: c

     pmix_proc_stats_t* PMIx_Proc_stats_create(size_t n);

* Free memory stored inside an array of proc stats structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Proc_stats_free(pmix_proc_stats_t *p, size_t n);

Disk Stats Functions
--------------------
* Initialize a disk stats struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Disk_stats_construct(pmix_disk_stats_t *p);

* Clear memory inside a disk stats struct:

  .. code-block:: c

     void PMIx_Disk_stats_destruct(pmix_disk_stats_t *p);

* Create and initialize an array of disk stats structs:

  .. code-block:: c

     pmix_disk_stats_t* PMIx_Disk_stats_create(size_t n);

* Free memory stored inside an array of disk stats structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Disk_stats_free(pmix_disk_stats_t *p, size_t n);

Net Stats Functions
-------------------
* Initialize a net stats struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Net_stats_construct(pmix_net_stats_t *p);

* Clear memory inside a net stats struct:

  .. code-block:: c

     void PMIx_Net_stats_destruct(pmix_net_stats_t *p);

* Create and initialize an array of net stats structs:

  .. code-block:: c

     pmix_net_stats_t* PMIx_Net_stats_create(size_t n);

* Free memory stored inside an array of net stats structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Net_stats_free(pmix_net_stats_t *p, size_t n);

Process Data Functions
----------------------
* Initialize a pdata struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_Pdata_construct(pmix_pdata_t *p);

* Clear memory inside a pdata struct:

  .. code-block:: c

     void PMIx_Pdata_destruct(pmix_pdata_t *p);

* Create and initialize an array of pdata structs:

  .. code-block:: c

     pmix_pdata_t* PMIx_Pdata_create(size_t n);

* Free memory stored inside an array of pdata structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_Pdata_free(pmix_pdata_t *p, size_t n);

App Struct Functions
--------------------
* Initialize a ``pmix_app_t`` struct. Memory for the provided
  object must have previously been allocated or statically declared:

  .. code-block:: c

     void PMIx_App_construct(pmix_app_t *p);

* Clear memory inside an app struct:

  .. code-block:: c

     void PMIx_App_destruct(pmix_app_t *p);

* Create and initialize an array of app structs:

  .. code-block:: c

     pmix_app_t* PMIx_App_create(size_t n);

* Create and initialize an array of ``pmix_info_t`` structs
  in the provided ``pmix_app_t`` object:

  .. code-block:: c

     void PMIx_App_info_create(pmix_app_t *p, size_t n);

* Free memory stored inside an array of app structs (does
  not free the struct memory itself):

  .. code-block:: c

     void PMIx_App_free(pmix_app_t *p, size_t n);

* Free memory stored inside a ``pmix_app_t`` object

  .. code-block:: c

     void PMIx_App_release(pmix_app_t *p);

PMIx Info List Functions
------------------------
Constructing arrays of ``pmix_info_t`` for passing to an API can
be tedious since the ``pmix_info_t`` itself is not a "list object".
Since this is a very frequent operation, a set of APIs has been
provided that opaquely manipulates internal PMIx list structures
for this purpose. The user only need provide a ``void*`` pointer to
act as the caddy for the internal list object. The base functions
for these operations are in the Standard, but the following functions
have been added here:

* Retrieve the next ``pmix_info_t`` from the provided list, given
  the current pointer. Passing a ``NULL`` to the ``prev`` parameter
  will return the first object on the list. A ``NULL`` is returned
  upon reaching the end of the list:

    .. code-block:: c

       pmix_info_t* PMIx_Info_list_get_info(void *ptr, void *prev, void **next);

* Insert a `pmix_info_t`` struct into the provided list. This directly
  copies the contents of the provided ``pmix_info_t`` struct, preserving
  any included pointers. The object on the list is subsequently marked
  as ``persistent`` to avoid free'ing any objects pointed to in the struct:

    .. code-block:: c

       pmix_status_t PMIx_Info_list_insert(void *ptr, pmix_info_t *info);

* Prepend a value onto the provided list:

    .. code-block:: c

       pmix_status_t PMIx_Info_list_prepend(void *ptr,
                                            const char *key,
                                            const void *value,
                                            pmix_data_type_t type);


Pretty-Print Functions
-----------------------
The following pretty-print support APIs have been added:

* Print a ``pmix_value_cmp_t`` value

  .. code-block:: c

     const char* PMIx_Value_comparison_string(pmix_value_cmp_t cmp);

* Print the contents of a ``pmix_app_t`` struct. Note that the returned
  string must be free'd by the caller:

   .. code-block:: c

    char* PMIx_App_string(const pmix_app_t *app);

The following pretty-print support APIs have been slightly modified
to add a ``const`` qualifier to their input parameter:

  .. code-block:: c

     const char* PMIx_Get_attribute_string(const char *attribute);
     const char* PMIx_Get_attribute_name(const char *attrstring);
     char* PMIx_Info_string(const pmix_info_t *info);
     char* PMIx_Value_string(const pmix_value_t *value);

  This is not expected to cause any issues for users.

The following function has been added to return the ``pmix_status_t``
corresponding to the string name of the constant:

   .. code-block:: c

    pmix_status_t PMIx_Error_code(const char *errname);


Constants
---------

* ``PMIX_DATA_BUFFER``: data type for packing/unpacking of
  ``pmix_data_buffer_t`` objects
* ``PMIX_DISK_STATS``: data type for packing/unpacking of
  ``pmix_disk_stats_t`` objects
* ``PMIX_NET_STATS``: data type for packing/unpacking of
  ``pmix_net_stats_t`` objects
* ``PMIX_NODE_STATS``: data type for packing/unpacking of
  ``pmix_node_stats_t`` objects
* ``PMIX_PROC_STATS``: data type for packing/unpacking of
  ``pmix_proc_stats_t`` objects
* ``PMIX_ERR_JOB_EXE_NOT_FOUND``: specified executable not found
* ``PMIX_ERR_JOB_INSUFFICIENT_RESOURCES``: insufficient resources to
  spawn job
* ``PMIX_ERR_JOB_SYS_OP_FAILED``: system library operation failed
* ``PMIX_ERR_JOB_WDIR_NOT_FOUND``: specified working directory not
  found
* ``PMIX_READY_FOR_DEBUG``: event indicating job/proc is ready for
  debug (accompanied by ``PMIX_BREAKPOINT`` indicating where proc is
  waiting)
* ``PMIX_ERR_PROC_REQUESTED_ABORT``: process called ``PMIx_Abort``
* ``PMIX_ERR_PROC_KILLED_BY_CMD``: process was terminated by RTE
  command
* ``PMIX_ERR_PROC_FAILED_TO_START``: process failed to start
* ``PMIX_ERR_PROC_ABORTED_BY_SIG``: process aborted by signal (e.g.,
  segmentation fault)
* ``PMIX_ERR_PROC_SENSOR_BOUND_EXCEEDED``: process terminated due to
  exceeding a sensor boundary
* ``PMIX_ERR_EXIT_NONZERO_TERM``: process exited normally, but with a
  non-zero status
* ``PMIX_INFO_QUALIFIER``  (value: 0x00000008): Info is a qualifier to the primary value
* ``PMIX_INFO_PERSISTENT`` (value: 0x00000010): Do not release included value


.. note:: OpenPMIx version |opmix_ver| renamed the  ``PMIX_DEBUG_WAIT_FOR_NOTIFY``
          to ``PMIX_READY_FOR_DEBUG``. The prior name is retained as deprecated
          for backward compatibility.

Attributes
----------

.. list-table::
   :header-rows: 1

   * - Attribute
     - Type
     - Description

   * - ``PMIX_EXTERNAL_AUX_EVENT_BASE`` ``"pmix.evaux"``
     - ``(void*)``
     - event base to be used for auxiliary
       functions (e.g., capturing signals) that would
       otherwise interfere with the
       host
       
   * - ``PMIX_CONNECT_TO_SCHEDULER`` ``"pmix.cnct.sched"``
     - ``(bool)``
     - Connect to the system scheduler
       
   * - ``PMIX_BIND_PROGRESS_THREAD`` ``"pmix.bind.pt"``
     - ``(char*)``
     - Comma-delimited ranges of CPUs
       that the internal PMIx progress
       thread shall be bound to
         
   * - ``PMIX_BIND_REQUIRED`` ``"pmix.bind.reqd"``
     - ``(bool)``
     - Return error if the internal PMIx
       progress thread cannot be bound
           
   * - ``PMIX_COLOCATE_PROCS`` ``"pmix.colproc"``
     - ``(pmix_data_array_t*)``
     - Array of ``pmix_proc_t`` identifying the
       procs with which the new job's procs
       are to be colocated
       
   * - ``PMIX_COLOCATE_NPERPROC`` ``"pmix.colnum.proc"``
     - ``(uint16_t)``
     - Number of procs to colocate with
       each identified proc
       
   * - ``PMIX_COLOCATE_NPERNODE`` ``"pmix.colnum.node"``
     - ``(uint16_t)``
     - Number of procs to colocate on the
       node of each identified proc
       
   * - ``PMIX_EVENT_ONESHOT`` ``pmix.evone``
     - ``(bool)``
     - when registering, indicate that this
       event handler is to be deleted after
       being invoked

   * - ``PMIX_GROUP_ADD_MEMBERS`` ``pmix.grp.add``
     - ``(pmix_data_array_t*)``
     - Array of ``pmix_proc_t`` identifying
       procs that are not included in the
       membership specified in the procs
       array passed to the
       ``PMIx_Group_construct[_nb]()`` call,
       but are to be included in the final
       group. The identified procs will be
       sent an invitation to join the group
       during the construction procedure.
       This is used when some members of
       the proposed group do not know the
       full membership and therefore cannot
       include all members in the call to
       construct.
       
   * - ``PMIX_GROUP_LOCAL_CID`` ``pmix.grp.lclid``
     - ``(size_t)``
     - Local context ID for the specified
       process member of a group
       
   * - ``PMIX_GROUP_INFO`` ``pmix.grp.info``
     - ``pmix_data_array_t``
     - Array of pmix_info_t containing data
       that is to be shared across all
       members of a group during group
       construction

   * - ``PMIX_IOF_TAG_DETAILED_OUTPUT`` ``pmix.iof.tagdet``
     - ``(bool)``
     - Tag output with the
       [local jobid,rank][hostname:pid]
       and channel it comes from
       
   * - ``PMIX_IOF_TAG_FULLNAME_OUTPUT`` ``pmix.iof.tagfull``
     - ``(bool)``
     - Tag output with the [nspace,rank]
       and channel it comes from
       
   * - ``PMIX_LOG_AGG`` ``pmix.log.agg``
     - ``(bool)``
     - Whether to aggregate and prevent
       duplicate logging messages based
       on key value pairs.
         
   * - ``PMIX_LOG_KEY`` ``pmix.log.key``
     - ``(char*)``
     - key to a logging message
         
   * - ``PMIX_LOG_VAL`` ``pmix.log.val``
     - ``(char*)``
     - value to a logging message
         
   * - ``PMIX_MYSERVER_URI`` ``pmix.mysrvr.uri``
     - ``(char*)``
     - URI of this proc's listener socket
         
   * - ``PMIX_QUALIFIED_VALUE`` ``pmix.qual.val``
     - ``(pmix_data_array_t*)``
     - Value being provided consists of the
       primary key-value pair in first position,
       followed by one or more key-value
       qualifiers to be used when
       subsequently retrieving the primary
       value
         
   * - ``PMIX_WDIR_USER_SPECIFIED`` ``pmix.wdir.user``
     - ``(bool)``
     - User specified the working directory
         
   * - ``PMIX_RUNTIME_OPTIONS`` ``pmix.runopt``
     - ``(char*)``
     - Environment-specific runtime
       directives that control job behavior
         
   * - ``PMIX_ABORT_NON_ZERO_TERM`` ``pmix.abnz``
     - ``(bool)``
     - Abort the spawned job if any process
       terminates with non-zero status
         
   * - ``PMIX_DO_NOT_LAUNCH`` ``pmix.dnl``
     - ``(bool)``
     - Execute all procedures to prepare the
       requested job for launch, but do not
       launch it. Typically combined with the
       PMIX_DISPLAY_MAP or
       PMIX_DISPLAY_MAP_DETAILED for
       debugging purposes.
         
   * - ``PMIX_SHOW_LAUNCH_PROGRESS`` ``pmix.showprog``
     - ``(bool)``
     - Provide periodic progress reports on
       job launch procedure (e.g., after
       every 100 processes have been
       spawned)
         
   * - ``PMIX_AGGREGATE_HELP`` ``pmix.agg.help``
     - ``(bool)``
     - Aggregate help messages, reporting
       each unique help message once
       accompanied by the number of
       processes that reported it
         
   * - ``PMIX_REPORT_CHILD_SEP`` ``pmix.rptchildsep``
     - ``(bool)``
     - Report the exit status of any child
       jobs spawned by the primary job
       separately. If false, then the final
       exit status reported will be zero if the
       primary job and all spawned jobs exit
       normally, or the first non-zero status
       returned by either primary or child
       jobs.
         
   * - ``PMIX_DISPLAY_MAP_DETAILED`` ``pmix.dispmapdet``
     - ``(bool)``
     - display a highly detailed placement
       map upon spawn
       
   * - ``PMIX_DISPLAY_ALLOCATION`` ``pmix.dispalloc``
     - ``(bool)``
     - display the resource allocation
         
   * - ``PMIX_DISPLAY_TOPOLOGY`` ``pmix.disptopo``
     - ``(char*)``
     - comma-delimited list of hosts whose
       topology is to be displayed
         
   * - ``PMIX_DISPLAY_PROCESSORS`` ``pmix.dispcpus``
     - ``(char*)``
     - comma-delimited list of hosts whose
       available CPUs are to be displayed

   * - ``PMIX_DISPLAY_PARSEABLE_OUTPUT`` ``pmix.dispparse``
     - ``(bool)``
     - display requested info in a format
       more amenable to machine parsing

   * - ``PMIX_SORTED_PROC_ARRAY`` ``pmix.sorted.parr``
     - ``(bool)``
     - Proc array being passed has been
       sorted
         
   * - ``PMIX_QUERY_PROVISIONAL_ABI_VERSION`` ``pmix.qry.prabiver``
     - ``(char*)``
     - The PMIx Standard Provisional ABI
       version(s) supported, returned in the
       form of a comma separated list of
       "MAJOR.MINOR" pairs

   * - ``PMIX_QUERY_STABLE_ABI_VERSION`` ``pmix.qry.stabiver``
     - ``(char*)``
     - The PMIx Standard Stable ABI
       version(s) supported, returned in the
       form of a comma separated list of
       "MAJOR.MINOR" pairs

.. note:: The attribute ``PMIX_DEBUG_STOP_IN_APP`` has been modified
          to only support a ``PMIX_BOOL`` value instead of an optional
          array of ranks due to questions over the use-case calling
          for stopping a subset of a job's processes while allowing
          others to run "free".

Datatypes
---------

* ``pmix_value_cmp_t``: an enum indicating the relative value of
  two ``pmix_value_t objects``. Values include:

  * ``PMIX_EQUAL``
  * ``PMIX_VALUE1_GREATER``
  * ``PMIX_VALUE2_GREATER``
  * ``PMIX_VALUE_TYPE_DIFFERENT``
  * ``PMIX_VALUE_INCOMPATIBLE_OBJECTS``
  * ``PMIX_VALUE_COMPARISON_NOT_AVAIL``

* ``pmix_boolean_t``: an enum indicating boolean state of a
  ``pmix_value_t`` (possibly contained in a ``pmix_info_t`` object):

  * ``PMIX_BOOL_TRUE``
  * ``PMIX_BOOL_FALSE``
  * ``PMIX_NON_BOOL``

* ``pmix_disk_stats_t``: contains statistics on disk read/write operations
* ``pmix_net_stats_t``: contains statistics on network activity
* ``pmix_node_stats_t``: contains statistics on node resource usage
* ``pmix_proc_stats_t``: contains statistics on process resource usage


Datatype static initializers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Static initializers were added for each complex data type (i.e., a data type
defined as a struct). Most are contained in the Standard, but the following
extensions have been provided:

* ``PMIX_PROC_STATS_STATIC_INIT``
* ``PMIX_DISK_STATS_STATIC_INIT``
* ``PMIX_NET_STATS_STATIC_INIT``
* ``PMIX_NODE_STATS_STATIC_INIT``


Macros
------
Although the convenience macros have been deprecated, several were
added (in deprecated form) that previously were missing. These
are added for symmetry to support those who continue to use
the macros, and include:

* ``PMIX_XFER_PROCID``: transfer a ``pmix_proc_t`` to another one
  (non-destructive copy)
* ``PMIX_INFO_SET_END``: mark this ``pmix_info_t`` as being at the end
  of an array
* ``PMIX_INFO_SET_PERSISTENT``: mark that the data in this
  ``pmix_info_t`` is not to be released by ``PMIX_Info_destruct()`` (or its
  macro form)
* ``PMIX_INFO_SET_QUALIFIER``: mark this ``pmix_info_t`` as a qualifier to the
  primary key
* ``PMIX_INFO_IS_PERSISTENT``: test if this ``pmix_info_t`` has been marked as persistent
* ``PMIX_INFO_IS_QUALIFIER``: test if this ``pmix_info_t`` has been marked as a qualifier
* ``PMIX_DATA_ARRAY_INIT``: initialize a ``pmix_data_array_t``
* ``PMIX_CHECK_TRUE``: check if a ``pmix_value_t`` is boolean ``true`` (supports
  string as well as traditional boolean values)
* ``PMIX_CHECK_BOOL``: check if a ``pmix_value_t`` is a boolean value (supports
  string as well as traditional boolean values)


Macros supporting ``pmix_disk_stats_t`` objects:

* ``PMIX_DISK_STATS_CONSTRUCT``
* ``PMIX_DISK_STATS_CREATE``
* ``PMIX_DISK_STATS_DESTRUCT``
* ``PMIX_DISK_STATS_FREE``
* ``PMIX_DISK_STATS_RELEASE``

Macros supporting ``pmix_net_stats_t`` objects:

* ``PMIX_NET_STATS_CONSTRUCT``
* ``PMIX_NET_STATS_CREATE``
* ``PMIX_NET_STATS_DESTRUCT``
* ``PMIX_NET_STATS_FREE``
* ``PMIX_NET_STATS_RELEASE``

Macros supporting ``pmix_node_stats_t`` objects:

* ``PMIX_NODE_STATS_CONSTRUCT``
* ``PMIX_NODE_STATS_CREATE``
* ``PMIX_NODE_STATS_DESTRUCT``
* ``PMIX_NODE_STATS_RELEASE``

Macros supporting ``pmix_proc_stats_t`` objects:

* ``PMIX_PROC_STATS_CONSTRUCT``
* ``PMIX_PROC_STATS_CREATE``
* ``PMIX_PROC_STATS_DESTRUCT``
* ``PMIX_PROC_STATS_FREE``
* ``PMIX_PROC_STATS_RELEASE``


Scheduler Integration
---------------------
OpenPMIx has taken some initial steps towards supporting the
integration of schedulers to runtime environments (RTEs) using
PMIx as the middleware. Supporting definitions will continue
to be added going forward. This section describes the current
state of those definitions.

Session Control Function
^^^^^^^^^^^^^^^^^^^^^^^^
Used by the scheduler to request a session control action by the RTE - e.g.,
setup a session (allocate the specified nodes to the new session,
provision the nodes with the specified image,
setup a user-level DVM across those nodes, and startup the given
application under control of that DVM). In addition to setting
up a new session, the function can be called to direct that a
currently executing session be preempted or terminated.
The sessionID identifies the
session to which the specified control action is to be applied. A
``UINT32_MAX`` value can be used to indicate all sessions under the
caller's control.

Also used by the RTE to report a change in session state - e.g.,
that the session has completed
The directives are provided as ``pmix_info_t`` structs in the
directives array. The callback function provides a status to
indicate whether or not the request was granted, and to provide some
information as to the reason for any denial in the
``pmix_info_cbfunc_t`` array of ``pmix_info_t`` structures. If
non-``NULL``, then the specified release_fn must be called when the
callback function completes |mdash| this will be used to release any
provided ``pmix_info_t`` array.

Passing ``NULL`` as the ``cbfunc`` to this call indicates that it shall
be treated as a blocking operation, with the return status
indicative of the overall operation's completion.

  .. code-block:: c

     pmix_status_t PMIx_Session_control(uint32_t sessionID,
                                        const pmix_info_t directives[], size_t ndirs,
                                        pmix_info_cbfunc_t cbfunc, void *cbdata);

Session Control Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^
Schedulers calling to create a session are required to provide:

* the effective userID and groupID that the session should have
  when instantiated.

* description of the resources that are to be included in the session

* if applicable, the image that should be provisioned on nodes
  included in the session

* an array of applications (if any) that are to be started in the
  session once instantiated

Attributes supported by this API when called by the scheduler include:

.. list-table::
   :header-rows: 1

   * - Attribute
     - Type
     - Description

   * - ``PMIX_SESSION_APP`` ``pmix.ssn.app``
     - ``(pmix_data_array_t*)``
     - Array of ``pmix_app_t`` to be executed
       in the assigned session upon session
       instantiation

   * - ``PMIX_SESSION_PROVISION`` ``pmix.ssn.pvn``
     - ``(pmix_data_array_t*)``
     - description of nodes to be
       provisioned with specified image

   * - ``PMIX_SESSION_PROVISION_NODES`` ``pmix.ssn.pvnnds``
     - ``(char*)``
     - regex identifying nodes that are to be
       provisioned

   * - ``PMIX_SESSION_PROVISION_IMAGE`` ``pmix.ssn.pvnimg``
     - ``(char*)``
     - name of the image that is to be
       provisioned

   * - ``PMIX_SESSION_PAUSE`` ``pmix.ssn.pause``
     - ``(bool)``
     - pause all jobs in the specified session

   * - ``PMIX_SESSION_RESUME`` ``pmix.ssn.resume``
     - ``(bool)``
     - "un-pause" all jobs in the specified session

   * - ``PMIX_SESSION_TERMINATE`` ``pmix.ssn.terminate``
     - ``(bool)``
     - terminate all jobs in the specified
       session and recover all resources
       included in the session.

   * - ``PMIX_SESSION_PREEMPT`` ``pmix.ssn.preempt``
     - ``(bool)``
     - preempt indicated jobs (given in
       accompanying ``pmix_info_t`` via the
       ``PMIX_NSPACE`` attribute) in the specified
       session and recover all their resources. If
       no ``PMIX_NSPACE`` is specified, then preempt
       all jobs in the session.

   * - ``PMIX_SESSION_RESTORE`` ``pmix.ssn.restore``
     - ``(bool)``
     - restore indicated jobs (given in
       accompanying ``pmix_info_t`` via the
       ``PMIX_NSPACE`` attribute) in the specified
       session, including all their resources. If
       no ``PMIX_NSPACE`` is specified, then restore
       all jobs in the session.

   * - ``PMIX_SESSION_SIGNAL`` ``pmix.ssn.sig``
     - ``(int)``
     - send given signal to all processes of every
       job in the session


Attributes supported by this API when called by the RTE include:

.. list-table::
   :header-rows: 1

   * - Attribute
     - Type
     - Description

   * - ``PMIX_SESSION_COMPLETE`` ``pmix.ssn.complete``
     - ``(bool)``
     - specified session has completed, all resources have been
       recovered and are available for scheduling. Must include
       ``pmix_info_t`` indicating ID and returned status of any jobs
       executing in the session.


Server module function pointers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The PMIx server module was extended to include the following interface
that is used by the PMIx server to pass control requests received
by the RTE from the scheduler. These include requests to establish
a newly allocated session, preempt jobs, etc. A ``UINT32_MAX`` value
for the sessionID indicates that the specified action shall be
applied to all currently existing sessions.


* Provide a session control operation request

  .. code-block:: c

    typedef pmix_status_t (*pmix_server_session_control_fn_t)(
                                  const pmix_proc_t *requestor,
                                  uint32_t sessionID,
                                  const pmix_info_t directives[], size_t ndirs,
                                  pmix_info_cbfunc_t cbfunc, void *cbdata);


Server module attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^
A number of allocation related attributes have already been defined
in the Standard. These can be used to describe the request (e.g., the
resources to be included in the session). The following
attribute has been added to that list:

.. list-table::
   :header-rows: 1

   * - Attribute
     - Type
     - Description

   * - ``PMIX_SESSION_CTRL_ID`` ``pmix.ssnctrl.id``
     - ``(char*)``
     - provide a string identifier for this request.
       This identifier shall be included
       in all subsequent interactions relating to
       the request.


Scheduler query attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^
The scheduler typically discovers its available resources
by querying the RTE for a list of them. The following
attributes augment those already in the Standard to support
the query:

.. list-table::
   :header-rows: 1

   * - Attribute
     - Type
     - Description

   * - ``PMIX_QUERY_ALLOCATION`` ``pmix.query.allc``
     - ``(pmix_data_array_t*)``
     - returns an array of ``pmix_info_t``
       describing the nodes known to the
       server. Each array element will consist
       of the ``PMIX_NODE_INFO`` key containing
       a ``pmix_data_array_t`` of ``pmix_info_t``.
       The first element of the array must be
       the hostname of that node, with
       additional info on the node in
       subsequent entries.
       SUPPORTED_QUALIFIER: a
       ``PMIX_ALLOC_ID`` qualifier indicating
       the specific allocation of interest

   * - ``PMIX_TOPOLOGY_INDEX`` ``pmix.topo.index``
     - ``(int)``
     - index of a topology in a storage array. Used
       when returning an allocation to avoid duplicate
       topology information - the RTE can return an array
       of topologies and then indicate the index
       to the topology as part of each node entry.


Allocation attributes
^^^^^^^^^^^^^^^^^^^^^
A number of allocation related attributes have already been defined
in the Standard. These can be used to describe the request (e.g., the
number and type of resources being requested). The following
attribute has been added to that list:

.. list-table::
   :header-rows: 1

   * - Attribute
     - Type
     - Description

   * - ``PMIX_ALLOC_PREEMPTIBLE`` ``pmix.alloc.preempt``
     - ``(bool)``
     - by default, all jobs in the resulting
       allocation are to be considered
       preemptible (can be overridden at
       per-job level)


Allocation directive values
^^^^^^^^^^^^^^^^^^^^^^^^^^^
The ``PMIx_Allocation_request`` API includes a ``directive`` parameter to
specify the operation being requested. These values were extended to include:

* ``PMIX_ALLOC_REQ_CANCEL`` (value: 5): Cancel the indicated allocation request

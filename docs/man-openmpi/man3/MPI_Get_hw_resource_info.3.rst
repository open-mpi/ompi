.. _mpi_get_hw_resource_info:


MPI_Get_hw_resource_info
========================

.. include_body

:ref:`MPI_Get_hw_resource_info` |mdash| Returns information about the hardware
resources on which the calling process can execute.

.. The following file was automatically generated
.. include:: ./bindings/mpi_get_hw_resource_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: Info object containing local hardware resource information
  (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Get_hw_resource_info` returns an info object describing hardware
resource types associated with the calling \MPI/ process at the moment of the
call. The application is responsible for freeing the returned info object with
:ref:`MPI_Info_free`.

Open MPI obtains this information from hwloc. Each key uses the URI form
``hwloc://<resource-type>``. The value is ``true`` if the calling process is
restricted to a single instance of that resource type and ``false`` if its CPU
binding spans multiple instances. Depending on the local topology, the returned
keys can include:

* ``hwloc://NUMANode``
* ``hwloc://Package``
* ``hwloc://L3Cache``
* ``hwloc://L2Cache``
* ``hwloc://L1Cache``
* ``hwloc://Core``
* ``hwloc://PU``

Resource types absent from the local topology are omitted. Open MPI returns an
empty info object if hardware topology or process binding information is not
available. The routine may be called before :ref:`MPI_Init` and after
:ref:`MPI_Finalize`; Open MPI returns an empty info object outside the active
MPI lifetime.

The returned keys can be passed as values of the ``mpi_hw_resource_type`` info
key to :ref:`MPI_Comm_split_type` with ``MPI_COMM_TYPE_HW_GUIDED`` or
``MPI_COMM_TYPE_RESOURCE_GUIDED``. A process receives ``MPI_COMM_NULL`` if its
current binding is unavailable or spans multiple instances of the requested
resource. For example:

.. code-block:: c

  MPI_Info hw_info;
  MPI_Comm hw_comm;
  int      nb_keys  = 0, flag = 0;
  int      is_found = 0, is_restricted = 0; 
  int      valuelen = 6; // max length between "false" and "true" + 1
  char    *value    = calloc(valuelen, sizeof(char));
  char    *hw_type  = calloc((MPI_MAX_INFO_KEY+1), sizeof(char));
  
  MPI_Get_hw_resource_info(&hw_info);
  
  MPI_Info_get_nkeys(hw_info, &nb_keys);
  for(int index = 0 ; index < nb_keys ; index++){ 
    MPI_Info_get_nthkey(hw_info, index, hw_type);
    MPI_Info_get_string(hw_info, hw_type, &valuelen, value, &flag);     
    if(strcmp(hw_type, "hwloc://NUMANode") == 0){
      is_found = 1;
      if(strcmp(value,"true") == 0)
        is_restricted = 1; 
      break; // Resource of type NUMANode found
    }
  } 

  // The calling MPI process is restricted to a resource
  // of the chosen type (NUMANode)
  if(is_found  && is_restricted){
    MPI_Info split_info;
    int rank;
    
    MPI_Info_create(&split_info);
     
    // hw_type now serves as value for the "mpi_hw_resource_type" key
    MPI_Info_set(split_info, "mpi_hw_resource_type", hw_type);
  
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_RESOURCE_GUIDED,
                        rank, split_info, &hw_comm);

    // Check and use hw_comm from this point if it's a valid
    // communicator or different from MPI_COMM_SELF or MPI_COMM_WORLD.
  } else {
    // If resource is not found or not restricted to it,
    // the calling MPI process does not participate to the call
    // hence the use of MPI_UNDEFINED as split_type and
    // MPI_COMM_NULL is produced as output communicator

    MPI_Comm_split_type(MPI_COMM_WORLD, MPI_UNDEFINED,
                        -1, MPI_INFO_NULL, &hw_comm);
  } 

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Comm_split_type`

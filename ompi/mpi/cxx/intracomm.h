// -*- c++ -*-
//
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

class Intracomm : public Comm {
public:

  // construction
  Intracomm() { }
  // copy
  Intracomm(const Comm_Null& data) : Comm(data) { }
  // inter-language operability

#if 0 /* OMPI_ENABLE_MPI_PROFILING */
  //NOTE: it is extremely important that Comm(data) happens below
  //  because there is a not only pmpi_comm in this Intracomm but
  //  there is also a pmpi_comm in the inherited Comm part. Both
  //  of these pmpi_comm's need to be initialized with the same
  //  MPI_Comm object. Also the assignment operators must take this
  //  into account.
  Intracomm(const Intracomm& data) : Comm(data), pmpi_comm(data) { }

  Intracomm(const MPI_Comm& data) : Comm(data), pmpi_comm(data) { }
  
  Intracomm(const PMPI::Intracomm& data) 
    : Comm((const PMPI::Comm&)data), pmpi_comm(data) { }

  // assignment
  Intracomm& operator=(const Intracomm& data) {
    Comm::operator=(data);
    pmpi_comm = data.pmpi_comm; 
    return *this;
  }
  Intracomm& operator=(const Comm_Null& data) {
    Comm::operator=(data);
    pmpi_comm = (PMPI::Intracomm)data; return *this;
  }
  // inter-language operability
  Intracomm& operator=(const MPI_Comm& data) {
    Comm::operator=(data);
    pmpi_comm = data;
    return *this;
  }

#else
  Intracomm(const Intracomm& data) : Comm(data.mpi_comm) { }

  inline Intracomm(const MPI_Comm& data);

  // assignment
  Intracomm& operator=(const Intracomm& data) {
    mpi_comm = data.mpi_comm; return *this;
  }

  Intracomm& operator=(const Comm_Null& data) {
    mpi_comm = data; return *this;
  }

  // inter-language operability
  Intracomm& operator=(const MPI_Comm& data) {
    mpi_comm = data; return *this; } 

#endif

  //
  // Collective Communication
  //

  virtual void
  Barrier() const;

  virtual void
  Bcast(void *buffer, int count, 
	const Datatype& datatype, int root) const;
  
  virtual void
  Gather(const void *sendbuf, int sendcount, 
	 const Datatype & sendtype, 
	 void *recvbuf, int recvcount, 
	 const Datatype & recvtype, int root) const;
  
  virtual void
  Gatherv(const void *sendbuf, int sendcount, 
	  const Datatype & sendtype, void *recvbuf, 
	  const int recvcounts[], const int displs[], 
	  const Datatype & recvtype, int root) const;
  
  virtual void
  Scatter(const void *sendbuf, int sendcount, 
	  const Datatype & sendtype, 
	  void *recvbuf, int recvcount, 
	  const Datatype & recvtype, int root) const;
  
  virtual void
  Scatterv(const void *sendbuf, const int sendcounts[], 
	   const int displs[], const Datatype & sendtype,
	   void *recvbuf, int recvcount, 
	   const Datatype & recvtype, int root) const;
  
  virtual void
  Allgather(const void *sendbuf, int sendcount, 
	    const Datatype & sendtype, void *recvbuf, 
	    int recvcount, const Datatype & recvtype) const;
  
  virtual void
  Allgatherv(const void *sendbuf, int sendcount, 
	     const Datatype & sendtype, void *recvbuf, 
	     const int recvcounts[], const int displs[],
	     const Datatype & recvtype) const;
  
  virtual void
  Alltoall(const void *sendbuf, int sendcount, 
	   const Datatype & sendtype, void *recvbuf, 
	   int recvcount, const Datatype & recvtype) const;
  
  virtual void
  Alltoallv(const void *sendbuf, const int sendcounts[], 
	    const int sdispls[], const Datatype & sendtype, 
	    void *recvbuf, const int recvcounts[], 
	    const int rdispls[], const Datatype & recvtype) const;
  
  virtual void
  Alltoallw(const void *sendbuf, const int sendcounts[],
            const int sdispls[], const Datatype sendtypes[],
            void *recvbuf, const int recvcounts[],
            const int rdispls[], const Datatype recvtypes[]) const;
  
  virtual void
  Reduce(const void *sendbuf, void *recvbuf, int count, 
	 const Datatype & datatype, const Op & op, 
	 int root) const;
  
  
  virtual void
  Allreduce(const void *sendbuf, void *recvbuf, int count,
	    const Datatype & datatype, const Op & op) const;
  
  virtual void
  Reduce_scatter(const void *sendbuf, void *recvbuf, 
		 int recvcounts[], 
		 const Datatype & datatype, 
		 const Op & op) const;
  
  virtual void
  Scan(const void *sendbuf, void *recvbuf, int count, 
       const Datatype & datatype, const Op & op) const;

  virtual void
  Exscan(const void *sendbuf, void *recvbuf, int count,
	 const Datatype & datatype, const Op & op) const;
  
  Intracomm
  Dup() const;
  

  virtual Intracomm& Clone() const;

  virtual Intracomm
  Create(const Group& group) const;
  
  virtual Intracomm
  Split(int color, int key) const;

  virtual Intercomm
  Create_intercomm(int local_leader, const Comm& peer_comm,
		   int remote_leader, int tag) const;
  
  virtual Cartcomm
  Create_cart(int ndims, const int dims[],
	      const bool periods[], bool reorder) const;
  
  virtual Graphcomm
  Create_graph(int nnodes, const int index[],
	       const int edges[], bool reorder) const;


  //
  // Process Creation and Management
  //
 
  virtual Intercomm Accept(const char* port_name, const Info& info, int root)
    const;

  virtual Intercomm Connect(const char* port_name, const Info& info, int root)
    const;

  virtual Intercomm Spawn(const char* command, const char* argv[],
                          int maxprocs, const Info& info, int root) const;

  virtual Intercomm Spawn(const char* command, const char* argv[],
                          int maxprocs, const Info& info,
                          int root, int array_of_errcodes[]) const;

  virtual Intercomm Spawn_multiple(int count, const char* array_of_commands[],
                                   const char** array_of_argv[],
                                   const int array_of_maxprocs[],
                                   const Info array_of_info[], int root);

  virtual Intercomm Spawn_multiple(int count, const char* array_of_commands[],
                                   const char** array_of_argv[],
                                   const int array_of_maxprocs[],
                                   const Info array_of_info[], int root,
                                   int array_of_errcodes[]);


  //#if 0 /* OMPI_ENABLE_MPI_PROFILING */
  //  virtual const PMPI::Comm& get_pmpi_comm() const { return pmpi_comm; }
  //#endif
protected:


#if 0 /* OMPI_ENABLE_MPI_PROFILING */
  PMPI::Intracomm pmpi_comm;
#endif

public: // JGS see above about friend decls
#if ! 0 /* OMPI_ENABLE_MPI_PROFILING */
  static Op* current_op;
#endif

};

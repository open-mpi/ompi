// -*- c++ -*-
//
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2005 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
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

inline void
MPI::Intercomm::Barrier() const
{
  (void)MPI_Barrier(mpi_comm);
}

inline void
MPI::Intercomm::Bcast(void *buffer, int count, 
      const MPI::Datatype& datatype, int root) const
{ 
  (void)MPI_Bcast(buffer, count, datatype, root, mpi_comm);
}

inline void
MPI::Intercomm::Gather(const void *sendbuf, int sendcount, 
			      const MPI::Datatype & sendtype, 
			      void *recvbuf, int recvcount, 
			      const MPI::Datatype & recvtype, int root) const
{
  (void)MPI_Gather(const_cast<void *>(sendbuf), sendcount, sendtype,
		   recvbuf, recvcount, recvtype, root, mpi_comm);
}

inline void
MPI::Intercomm::Gatherv(const void *sendbuf, int sendcount, 
	const MPI::Datatype & sendtype, void *recvbuf, 
	const int recvcounts[], const int displs[], 
	const MPI::Datatype & recvtype, int root) const
{
  (void)MPI_Gatherv(const_cast<void *>(sendbuf), sendcount,  sendtype,
		    recvbuf, const_cast<int *>(recvcounts), const_cast<int *>(displs), 
		    recvtype, root, mpi_comm);
}

inline void
MPI::Intercomm::Scatter(const void *sendbuf, int sendcount, 
	const MPI::Datatype & sendtype, 
	void *recvbuf, int recvcount, 
	const MPI::Datatype & recvtype, int root) const
{ 
  (void)MPI_Scatter(const_cast<void *>(sendbuf), sendcount, sendtype,
		    recvbuf, recvcount, recvtype, root, mpi_comm);
}

inline void
MPI::Intercomm::Scatterv(const void *sendbuf, const int sendcounts[], 
	 const int displs[], const MPI::Datatype & sendtype,
	 void *recvbuf, int recvcount, 
	 const MPI::Datatype & recvtype, int root) const
{
  (void)MPI_Scatterv(const_cast<void *>(sendbuf), const_cast<int *>(sendcounts), 
		     const_cast<int *>(displs), sendtype, 
		     recvbuf, recvcount, recvtype, 
		     root, mpi_comm);
}

inline void
MPI::Intercomm::Allgather(const void *sendbuf, int sendcount, 
	  const MPI::Datatype & sendtype, void *recvbuf, 
	  int recvcount, const MPI::Datatype & recvtype) const 
{
  (void)MPI_Allgather(const_cast<void *>(sendbuf), sendcount, 
		      sendtype, recvbuf, recvcount,
		      recvtype, mpi_comm);
}

inline void
MPI::Intercomm::Allgatherv(const void *sendbuf, int sendcount, 
	   const MPI::Datatype & sendtype, void *recvbuf, 
	   const int recvcounts[], const int displs[],
	   const MPI::Datatype & recvtype) const
{
  (void)MPI_Allgatherv(const_cast<void *>(sendbuf), sendcount, 
		       sendtype, recvbuf, 
		       const_cast<int *>(recvcounts), const_cast<int *>(displs), 
		       recvtype, mpi_comm);
}

inline void
MPI::Intercomm::Alltoall(const void *sendbuf, int sendcount, 
	 const MPI::Datatype & sendtype, void *recvbuf, 
	 int recvcount, const MPI::Datatype & recvtype) const
{
  (void)MPI_Alltoall(const_cast<void *>(sendbuf), sendcount,
		     sendtype, recvbuf, recvcount,
		     recvtype, mpi_comm);
}

inline void
MPI::Intercomm::Alltoallv(const void *sendbuf, const int sendcounts[], 
	  const int sdispls[], const MPI::Datatype & sendtype, 
	  void *recvbuf, const int recvcounts[], 
	  const int rdispls[], const MPI::Datatype & recvtype) const 
{
    (void)MPI_Alltoallv(const_cast<void *>(sendbuf), 
                        const_cast<int *>(sendcounts), 
			const_cast<int *>(sdispls), sendtype, recvbuf, 
			const_cast<int *>(recvcounts), 
                        const_cast<int *>(rdispls), 
			recvtype,mpi_comm);
}

inline void
MPI::Intercomm::Alltoallw(const void *sendbuf, const int sendcounts[],
	const int sdispls[], const MPI::Datatype sendtypes[],
	void *recvbuf, const int recvcounts[],
	const int rdispls[], const MPI::Datatype recvtypes[]) const
{
    const int comm_size = Get_size();
    MPI_Datatype *const data_type_tbl = new MPI_Datatype [2*comm_size];

    // This must be done because MPI::Datatype arrays cannot be
    // converted directly into MPI_Datatype arrays.  
    for (int i_rank=0; i_rank < comm_size; i_rank++) {
        data_type_tbl[i_rank] = sendtypes[i_rank];
        data_type_tbl[i_rank + comm_size] = recvtypes[i_rank];
    }

    (void)MPI_Alltoallw(const_cast<void *>(sendbuf), 
                        const_cast<int *>(sendcounts),
                        const_cast<int *>(sdispls),
                        data_type_tbl, recvbuf,
                        const_cast<int *>(recvcounts), 
                        const_cast<int *>(rdispls),
                        &data_type_tbl[comm_size], mpi_comm);

    delete[] data_type_tbl;
}

inline void
MPI::Intercomm::Reduce(const void *sendbuf, void *recvbuf, int count, 
       const MPI::Datatype & datatype, const MPI::Op& op, 
       int root) const
{
  current_op = const_cast<MPI::Op*>(&op);
  (void)MPI_Reduce(const_cast<void *>(sendbuf), recvbuf, count, datatype, op, root, mpi_comm);
  current_op = (Op*)0;
}

inline void
MPI::Intercomm::Allreduce(const void *sendbuf, void *recvbuf, int count,
	  const MPI::Datatype & datatype, const MPI::Op& op) const
{
  current_op = const_cast<MPI::Op*>(&op);
  (void)MPI_Allreduce (const_cast<void *>(sendbuf), recvbuf, count, datatype,  op, mpi_comm);
  current_op = (Op*)0;
}

inline void
MPI::Intercomm::Reduce_scatter(const void *sendbuf, void *recvbuf, 
	       int recvcounts[], 
	       const MPI::Datatype & datatype, 
	       const MPI::Op& op) const
{
  current_op = const_cast<MPI::Op*>(&op);
  (void)MPI_Reduce_scatter(const_cast<void *>(sendbuf), recvbuf, recvcounts,
			   datatype, op, mpi_comm);
  current_op = (Op*)0;
}

inline MPI::Intercomm
MPI::Intercomm::Dup() const
{
  MPI_Comm newcomm;
  (void)MPI_Comm_dup(mpi_comm, &newcomm);
  return newcomm;
}

inline MPI::Intercomm&
MPI::Intercomm::Clone() const
{
  MPI_Comm newcomm;
  (void)MPI_Comm_dup(mpi_comm, &newcomm);
  MPI::Intercomm* dup = new MPI::Intercomm(newcomm);
  return *dup;
}

inline int
MPI::Intercomm::Get_remote_size() const 
{
  int size;
  (void)MPI_Comm_remote_size(mpi_comm, &size);
  return size;
}

inline MPI::Group
MPI::Intercomm::Get_remote_group() const 
{
  MPI_Group group;
  (void)MPI_Comm_remote_group(mpi_comm, &group);
  return group;
}

inline MPI::Intracomm
MPI::Intercomm::Merge(bool high)
{
  MPI_Comm newcomm;
  (void)MPI_Intercomm_merge(mpi_comm, (int)high, &newcomm);
  return newcomm;
}


//
// Extended Collective Operations
//

inline MPI::Intercomm
MPI::Intercomm::Create(const Group& group) const
{
  MPI_Comm newcomm;
  (void) MPI_Comm_create(mpi_comm, (MPI_Group) group, &newcomm);
  return newcomm;
}

inline MPI::Intercomm
MPI::Intercomm::Split(int color, int key) const
{
  MPI_Comm newcomm;
  (void) MPI_Comm_split(mpi_comm, color, key, &newcomm);
  return newcomm;
}

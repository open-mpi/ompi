// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

//
// Point-to-Point Communication
//

inline void
MPI::Request::Wait(MPI::Status &status) 
{
  (void)MPI_Wait(&mpi_request, &status.mpi_status);
}

inline void
MPI::Request::Wait() 
{
  (void)MPI_Wait(&mpi_request, MPI_STATUS_IGNORE);
}

inline void
MPI::Request::Free() 
{
  (void)MPI_Request_free(&mpi_request);
}

inline bool
MPI::Request::Test(MPI::Status &status) 
{
  int t;
  (void)MPI_Test(&mpi_request, &t, &status.mpi_status);
  return (bool) t;
}

inline bool
MPI::Request::Test() 
{
  int t;
  (void)MPI_Test(&mpi_request, &t, MPI_STATUS_IGNORE);
  return (bool) t;
}

inline int
MPI::Request::Waitany(int count, MPI::Request array[],
			     MPI::Status& status)
{
  int index, i;
  MPI_Request* array_of_requests = new MPI_Request[count];
  for (i=0; i < count; i++)
    array_of_requests[i] = array[i];
  (void)MPI_Waitany(count, array_of_requests, &index, &status.mpi_status);
  for (i=0; i < count; i++)
    array[i] = array_of_requests[i];
  delete [] array_of_requests;
  return index;
}

inline int
MPI::Request::Waitany(int count, MPI::Request array[])
{
  int index, i;
  MPI_Request* array_of_requests = new MPI_Request[count];
  for (i=0; i < count; i++)
    array_of_requests[i] = array[i];
  (void)MPI_Waitany(count, array_of_requests, &index, MPI_STATUS_IGNORE);
  for (i=0; i < count; i++)
    array[i] = array_of_requests[i];
  delete [] array_of_requests;
  return index; //JGS, Waitany return value
}

inline bool
MPI::Request::Testany(int count, MPI::Request array[],
			     int& index, MPI::Status& status)
{
  int i, flag;
  MPI_Request* array_of_requests = new MPI_Request[count];
  for (i=0; i < count; i++)
    array_of_requests[i] = array[i];
  (void)MPI_Testany(count, array_of_requests, &index, &flag, &status.mpi_status);
  for (i=0; i < count; i++)
    array[i] = array_of_requests[i];
  delete [] array_of_requests;
  return (bool)flag;
}

inline bool
MPI::Request::Testany(int count, MPI::Request array[], int& index)
{
  int i, flag;
  MPI_Request* array_of_requests = new MPI_Request[count];
  for (i=0; i < count; i++)
    array_of_requests[i] = array[i];
  (void)MPI_Testany(count, array_of_requests, &index, &flag, 
		    MPI_STATUS_IGNORE);
  for (i=0; i < count; i++)
    array[i] = array_of_requests[i];
  delete [] array_of_requests;
  return (bool)flag;
}

inline void
MPI::Request::Waitall(int count, MPI::Request req_array[],
			     MPI::Status stat_array[])
{
  int i;
  MPI_Request* array_of_requests = new MPI_Request[count];
  MPI_Status* array_of_statuses = new MPI_Status[count];
  for (i=0; i < count; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Waitall(count, array_of_requests, array_of_statuses);
  for (i=0; i < count; i++)
    req_array[i] = array_of_requests[i];
  for (i=0; i < count; i++)
    stat_array[i] = array_of_statuses[i];
  delete [] array_of_requests;
  delete [] array_of_statuses;
}

inline void
MPI::Request::Waitall(int count, MPI::Request req_array[])
{
  int i;
  MPI_Request* array_of_requests = new MPI_Request[count];

  for (i=0; i < count; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Waitall(count, array_of_requests, MPI_STATUSES_IGNORE);

  for (i=0; i < count; i++)
    req_array[i] = array_of_requests[i];

  delete [] array_of_requests;
} 

inline bool
MPI::Request::Testall(int count, MPI::Request req_array[],
			     MPI::Status stat_array[])
{
  int i, flag;
  MPI_Request* array_of_requests = new MPI_Request[count];
  MPI_Status* array_of_statuses = new MPI_Status[count];
  for (i=0; i < count; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Testall(count, array_of_requests, &flag, array_of_statuses);
  for (i=0; i < count; i++)
    req_array[i] = array_of_requests[i];
  for (i=0; i < count; i++)
    stat_array[i] = array_of_statuses[i];
  delete [] array_of_requests;
  delete [] array_of_statuses;
  return (bool) flag;
}

inline bool
MPI::Request::Testall(int count, MPI::Request req_array[])
{
  int i, flag;
  MPI_Request* array_of_requests = new MPI_Request[count];

  for (i=0; i < count; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Testall(count, array_of_requests, &flag, MPI_STATUSES_IGNORE);

  for (i=0; i < count; i++)
    req_array[i] = array_of_requests[i];
  delete [] array_of_requests;

  return (bool) flag;
} 

inline int
MPI::Request::Waitsome(int incount, MPI::Request req_array[],
			      int array_of_indices[], MPI::Status stat_array[]) 
{
  int i, outcount;
  MPI_Request* array_of_requests = new MPI_Request[incount];
  MPI_Status* array_of_statuses = new MPI_Status[incount];
  for (i=0; i < incount; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Waitsome(incount, array_of_requests, &outcount,
		     array_of_indices, array_of_statuses);
  for (i=0; i < incount; i++)
    req_array[i] = array_of_requests[i];
  for (i=0; i < incount; i++)
    stat_array[i] = array_of_statuses[i];
  delete [] array_of_requests;
  delete [] array_of_statuses;
  return outcount;
}

inline int
MPI::Request::Waitsome(int incount, MPI::Request req_array[],
			      int array_of_indices[]) 
{
  int i, outcount;
  MPI_Request* array_of_requests = new MPI_Request[incount];

  for (i=0; i < incount; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Waitsome(incount, array_of_requests, &outcount,
		     array_of_indices, MPI_STATUSES_IGNORE);

  for (i=0; i < incount; i++)
    req_array[i] = array_of_requests[i];
  delete [] array_of_requests;

  return outcount;
}

inline int
MPI::Request::Testsome(int incount, MPI::Request req_array[],
			      int array_of_indices[], MPI::Status stat_array[]) 
{
  int i, outcount;
  MPI_Request* array_of_requests = new MPI_Request[incount];
  MPI_Status* array_of_statuses = new MPI_Status[incount];
  for (i=0; i < incount; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Testsome(incount, array_of_requests, &outcount,
		     array_of_indices, array_of_statuses);
  for (i=0; i < incount; i++)
    req_array[i] = array_of_requests[i];
  for (i=0; i < incount; i++)
    stat_array[i] = array_of_statuses[i];
  delete [] array_of_requests;
  delete [] array_of_statuses;
  return outcount;
}

inline int
MPI::Request::Testsome(int incount, MPI::Request req_array[],
			      int array_of_indices[]) 
{
  int i, outcount;
  MPI_Request* array_of_requests = new MPI_Request[incount];

  for (i=0; i < incount; i++)
    array_of_requests[i] = req_array[i];
  (void)MPI_Testsome(incount, array_of_requests, &outcount,
		     array_of_indices, MPI_STATUSES_IGNORE);

  for (i=0; i < incount; i++)
    req_array[i] = array_of_requests[i];
  delete [] array_of_requests;

  return outcount;
}

inline void
MPI::Request::Cancel(void) const
{
  (void)MPI_Cancel((MPI_Request*)&mpi_request);
}

inline void
MPI::Prequest::Start()
{
  (void)MPI_Start(&mpi_request);
}

inline void
MPI::Prequest::Startall(int count, MPI:: Prequest array_of_requests[])
{
  //convert the array of Prequests to an array of MPI_requests
  MPI_Request* mpi_requests = new MPI_Request[count];
  int i;
  for (i=0; i < count; i++) {
    mpi_requests[i] = array_of_requests[i];
  }
  (void)MPI_Startall(count, mpi_requests); 
  for (i=0; i < count; i++)
    array_of_requests[i].mpi_request = mpi_requests[i] ;
  delete [] mpi_requests;
} 


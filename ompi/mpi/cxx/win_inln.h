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
// Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//



//
// Miscellany
//


inline MPI::Errhandler 
MPI::Win::Create_errhandler(MPI::Win::Errhandler_fn* function)
{
  MPI_Errhandler errhandler;
  (void) MPI_Win_create_errhandler((MPI_Win_errhandler_fn *) ompi_mpi_cxx_win_errhandler_intercept, 
				   &errhandler);
  MPI::Errhandler temp(errhandler);
  temp.win_handler_fn = (void(*)(MPI::Win&, int* , ...)) function;
  return temp;
}

       
inline MPI::Errhandler 
MPI::Win:: Get_errhandler() const 
{
    return *my_errhandler;
}


//
// One sided communication
//


inline void 
MPI::Win::Accumulate(const void* origin_addr, int origin_count,
	       	const MPI::Datatype& origin_datatype, int target_rank,
	       	MPI::Aint target_disp, int target_count,
	       	const MPI::Datatype& target_datatype,
	       	const MPI::Op& op) const 
{
  (void) MPI_Accumulate(const_cast<void *>(origin_addr), origin_count, origin_datatype,
			target_rank, target_disp, target_count, 
			target_datatype, op, mpi_win);
  
}


inline void
MPI::Win::Complete() const 
{
  (void) MPI_Win_complete(mpi_win);
}


inline  MPI::Win 
MPI::Win::Create(const void* base, MPI::Aint size, 
			int disp_unit, const MPI::Info& info, 
			const MPI::Intracomm& comm) 
{
  MPI_Win newwin;
  (void) MPI_Win_create(const_cast<void *>(base), size, disp_unit, info, comm, &newwin);
  return newwin;
}


inline void 
MPI::Win::Fence(int assert) const 
{
  (void) MPI_Win_fence(assert, mpi_win);
}


inline void 
MPI::Win::Get(const void *origin_addr, int origin_count, 
		     const MPI::Datatype& origin_datatype, 
		     int target_rank, MPI::Aint target_disp, 
		     int target_count, 
		     const MPI::Datatype& target_datatype) const 
{
  (void) MPI_Get(const_cast<void *>(origin_addr), origin_count, origin_datatype,
		     target_rank, target_disp, 
		     target_count, target_datatype, mpi_win);

}


inline MPI::Group 
MPI::Win::Get_group() const
{
  MPI_Group mpi_group;
  (void) MPI_Win_get_group(mpi_win, &mpi_group);
  return mpi_group;
}


inline void 
MPI::Win::Lock(int lock_type, int rank, int assert) const 
{
  (void) MPI_Win_lock(lock_type, rank, assert, mpi_win);
}


inline void 
MPI::Win::Post(const MPI::Group& group, int assert) const 
{
  (void) MPI_Win_post(group, assert, mpi_win);
}


inline void 
MPI::Win::Put(const void* origin_addr, int origin_count, 
		     const MPI::Datatype& origin_datatype, 
		     int target_rank, MPI::Aint target_disp, 
		     int target_count, 
		     const MPI::Datatype& target_datatype) const 
{
  (void) MPI_Put(const_cast<void *>(origin_addr), origin_count, origin_datatype, 
		 target_rank, target_disp, target_count,
		 target_datatype, mpi_win);
  
}


inline void 
MPI::Win::Start(const MPI::Group& group, int assert) const
{
  (void) MPI_Win_start(group, assert, mpi_win);
}


inline bool 
MPI::Win::Test() const
{
  int flag;
  MPI_Win_test(mpi_win, &flag);
  return OPAL_INT_TO_BOOL(flag);
  
}


inline void 
MPI::Win::Unlock(int rank) const
{
  (void) MPI_Win_unlock(rank, mpi_win);
}


inline void 
MPI::Win::Wait() const
{
  (void) MPI_Win_wait(mpi_win);
}


//
// External Interfaces
//

inline void 
MPI::Win::Call_errhandler(int errorcode) const
{
  (void) MPI_Win_call_errhandler(mpi_win, errorcode);
}


inline void 
MPI::Win::Delete_attr(int win_keyval) 
{
  (void) MPI_Win_delete_attr(mpi_win, win_keyval);
}


// version 1: pre-errata Get_attr (not correct, but probably nice to support
inline bool
MPI::Win::Get_attr(const Win& win, int win_keyval,
                         void* attribute_val) const
{
  int ret;
  (void) MPI_Win_get_attr(win, win_keyval, attribute_val, &ret);
  return OPAL_INT_TO_BOOL(ret);
}


// version 2: post-errata Get_attr (correct, but no one seems to know about it)
inline bool 
MPI::Win::Get_attr(int win_keyval, void* attribute_val) const
{
  int ret;
  (void) MPI_Win_get_attr(mpi_win, win_keyval, attribute_val, &ret);
  return OPAL_INT_TO_BOOL(ret);
}


inline void 
MPI::Win::Get_name(char* win_name, int& resultlen) const
{
  (void) MPI_Win_get_name(mpi_win, win_name, &resultlen);
}



inline void 
MPI::Win::Set_name(const char* win_name) 
{
  (void) MPI_Win_set_name(mpi_win, const_cast<char *>(win_name));
}


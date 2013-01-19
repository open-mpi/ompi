/* The program demonstrates how to set up and use a strided vector.
* The process with rank 0 creates a matrix. The columns of the
* matrix will then be distributed with a collective communication
* operation to all processes. Each process performs an operation on
* all column elements. Afterwards the results are collected in the
* source matrix overwriting the original column elements.
*
* The program uses between one and n processes to change the values
* of the column elements if the matrix has n columns. If you start
* the program with one process it has to work on all n columns alone
* and if you start it with n processes each process modifies the
* values of one column. Every process must know how many columns it
* has to modify so that it can allocate enough buffer space for its
* column block. Therefore the process with rank 0 computes the
* numbers of columns for each process in the array "num_columns" and
* distributes this array with MPI_Broadcast to all processes. Each
* process can now allocate memory for its column block. There is
* still one task to do before the columns of the matrix can be
* distributed with MPI_Scatterv: The size of every column block and
* the offset of every column block must be computed und stored in
* the arrays "sr_counts" and "sr_disps".
*
* An MPI data type is defined by its size, its contents, and its
* extent. When multiple elements of the same size are used in a
* contiguous manner (e.g. in a "scatter" operation or an operation
* with "count" greater than one) the extent is used to compute where
* the next element will start. The extent for a derived data type is
* as big as the size of the derived data type so that the first
* elements of the second structure will start after the last element
* of the first structure, i.e., you have to "resize" the new data
* type if you want to send it multiple times (count > 1) or to
* scatter/gather it to many processes. Restrict the extent of the
* derived data type for a strided vector in such a way that it looks
* like just one element if it is used with "count > 1" or in a
* scatter/gather operation.
*
* This version constructs a new column type (strided vector) with
* "MPI_Type_vector" and uses collective communication. The new
* data type knows the number of elements within one column and the
* spacing between two column elements. The program uses at most
* n processes if the matrix has n columns, i.e. depending on the
* number of processes each process receives between 1 and n columns.
* You can execute this program with an arbitrary number of processes
* because it creates its own group with "num_worker" (<= n) processes
* to perform the work if the matrix has n columns and the basic group
* contains too many processes.
*
*
* Compiling:
*   Store executable(s) into local directory.
*     mpicc -o <program name> <source code file name>
*
*   Store executable(s) into predefined directories.
*     make
*
*   Make program(s) automatically on all specified hosts. You must
*   edit the file "make_compile" and specify your host names before
*   you execute it.
*     make_compile
*
* Running:
*   LAM-MPI:
*     mpiexec -boot -np <number of processes> <program name>
*     or
*     mpiexec -boot \
*	 -host <hostname> -np <number of processes> <program name> : \
*	 -host <hostname> -np <number of processes> <program name>
*     or
*     mpiexec -boot [-v] -configfile <application file>
*     or
*     lamboot [-v] [<host file>]
*       mpiexec -np <number of processes> <program name>
*	 or
*	 mpiexec [-v] -configfile <application file>
*     lamhalt
*
*   OpenMPI:
*     "host1", "host2", and so on can all have the same name,
*     if you want to start a virtual computer with some virtual
*     cpu's on the local host. The name "localhost" is allowed
*     as well.
*
*     mpiexec -np <number of processes> <program name>
*     or
*     mpiexec --host <host1,host2,...> \
*	 -np <number of processes> <program name>
*     or
*     mpiexec -hostfile <hostfile name> \
*	 -np <number of processes> <program name>
*     or
*     mpiexec -app <application file>
*
* Cleaning:
*   local computer:
*     rm <program name>
*     or
*     make clean_all
*   on all specified computers (you must edit the file "make_clean_all"
*   and specify your host names before you execute it.
*     make_clean_all
*
*
* File: data_type_4.c			Author: S. Gross
* Date: 30.08.2012
*
*/

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

#define	P		6		/* # of rows			*/
#define Q		10		/* # of columns			*/
#define FACTOR		2		/* multiplicator for col. elem.	*/
#define DEF_NUM_WORKER	Q		/* # of workers, must be <= Q	*/

/* define macro to test the result of a "malloc" operation		*/
#define TestEqualsNULL(val)  \
 if (val == NULL) \
 { \
   fprintf (stderr, "file: %s  line %d: Couldn't allocate memory.\n", \
	     __FILE__, __LINE__); \
   exit (EXIT_FAILURE); \
 }

/* define macro to determine the minimum of two values			*/
#define MIN(a,b)	((a) < (b) ? (a) : (b))


static void print_matrix (int p, int q, double **mat);


int main (int argc, char *argv[])
{
 int    ntasks,			/* number of parallel tasks	*/
        mytid,				/* my task id			*/
        namelen,			/* length of processor name	*/
        i, j,				/* loop variables		*/
	 *num_columns,			/* # of columns in column block	*/
	 *sr_counts,			/* send/receive counts		*/
	 *sr_disps,			/* send/receive displacements	*/
	 tmp, tmp1;			/* temporary values		*/
 double matrix[P][Q],
   	 **col_block;			/* column block of matrix	*/
 char   processor_name[MPI_MAX_PROCESSOR_NAME];
 MPI_Datatype	column_t,		/* column type (strided vector)	*/
		col_block_t,
		tmp_column_t;		/* needed to resize the extent	*/
 MPI_Group	group_comm_world,	/* processes in "basic group"	*/
		group_worker,		/* processes in new groups	*/
		group_other;
 MPI_Comm	COMM_WORKER,		/* communicators for new groups	*/
		COMM_OTHER;
 int		num_worker,		/* # of worker in "group_worker"*/
		*group_w_mem,		/* array of worker members 	*/
		group_w_ntasks,		/* # of tasks in "group_worker"	*/
		group_o_ntasks,		/* # of tasks in "group_other"	*/
		group_w_mytid,		/* my task id in "group_worker"	*/
		group_o_mytid,		/* my task id in "group_other"	*/
		*universe_size_ptr,	/* ptr to # of "virtual cpu's"	*/
		universe_size_flag;	/* true if available		*/

 MPI_Init (&argc, &argv);
 MPI_Comm_rank (MPI_COMM_WORLD, &mytid);
 MPI_Comm_size (MPI_COMM_WORLD, &ntasks);
 /* Determine the correct number of processes for this program. If
  * there are more than Q processes (i.e., more processes than
  * columns) available, we split the "basic group" into two groups.
  * This program uses a group "group_worker" to do the real work
  * and a group "group_other" for the remaining processes of the
  * "basic group". The latter have nothing to do and can terminate
  * immediately. If there are less than or equal to Q processes
  * available all processes belong to group "group_worker" and group
  * "group_other" is empty. At first we find out which processes
  * belong to the "basic group".
  */
 MPI_Comm_group (MPI_COMM_WORLD, &group_comm_world);
 if (ntasks > Q)
 {
   /* There are too many processes, so that we must build a new group
    * with "num_worker" processes. "num_worker" will be the minimum of
    * DEF_NUM_WORKER and the "universe size" if it is supported by the
    * MPI implementation. At first we must check if DEF_NUM_WORKER has
    * a suitable value.
    */
   if (DEF_NUM_WORKER > Q)
   {
     if (mytid == 0)
     {
	fprintf (stderr, "\nError:\tInternal program error.\n"
		 "\tConstant DEF_NUM_WORKER has value %d but must be\n"
		 "\tlower than or equal to %d. Please change source\n"
		 "\tcode and compile the program again.\n\n",
		 DEF_NUM_WORKER, Q);
     }
     MPI_Group_free (&group_comm_world);
     MPI_Finalize ();
     exit (EXIT_FAILURE);
   }
   /* determine the universe size, set "num_worker" in an
    * appropriate way, and allocate memory for the array containing
    * the ranks of the members of the new group
    */
   MPI_Comm_get_attr (MPI_COMM_WORLD, MPI_UNIVERSE_SIZE,
		       &universe_size_ptr, &universe_size_flag);
   if ((universe_size_flag != 0) && (*universe_size_ptr > 0))
   {
     num_worker = MIN (DEF_NUM_WORKER, *universe_size_ptr);
   }
   else
   {
     num_worker = DEF_NUM_WORKER;
   }
   group_w_mem = (int *) malloc (num_worker * sizeof (int));
   TestEqualsNULL (group_w_mem);	/* test if memory was available	*/
   if (mytid == 0)
   {
     printf ("\nYou have started %d processes but I need at most "
	      "%d processes.\n"
	      "The universe contains %d \"virtual cpu's\" (\"0\" means "
	      "not supported).\n"
	      "I build a new worker group with %d processes. The "
	      "processes with\n"
	      "the following ranks in the basic group belong to "
	      "the new group:\n  ",
	      ntasks, Q, *universe_size_ptr, num_worker);
   }
   for (i = 0; i < num_worker; ++i)
   {
     /* fetch some ranks from the basic group for the new worker
      * group, e.g. the last num_worker ranks to demonstrate that
      * a process may have different ranks in different groups
      */
     group_w_mem[i] = (ntasks - num_worker) + i;
     if (mytid == 0)
     {
	printf ("%d   ", group_w_mem[i]);
     }
   }
   if (mytid == 0)
   {
     printf ("\n\n");
   }
   /* Create group "group_worker"					*/
   MPI_Group_incl (group_comm_world, num_worker, group_w_mem,
		    &group_worker);
   free (group_w_mem);
 }
 else
 {
   /* there are at most as many processes as columns in our matrix,
    * i.e., we can use the "basic group"
    */
   group_worker = group_comm_world;
 }
 /* Create group "group_other" which demonstrates only how to use
  * another group operation and which has  nothing to do in this
  * program.
  */
 MPI_Group_difference (group_comm_world, group_worker,
			&group_other);
 MPI_Group_free (&group_comm_world);
 /* Create communicators for both groups. The communicator is only
  * defined for all processes of the group and it is undefined
  * (MPI_COMM_NULL) for all other processes.
  */
 MPI_Comm_create (MPI_COMM_WORLD, group_worker, &COMM_WORKER);
 MPI_Comm_create (MPI_COMM_WORLD, group_other, &COMM_OTHER);


 /* =========================================================
  * ======						======
  * ======  Supply work for all different groups.	======
  * ======						======
  * ======						======
  * ====== At first you must find out if a process	======
  * ====== belongs to a special group. You can use	======
  * ====== MPI_Group_rank for this purpose. It returns	======
  * ====== the rank of the calling process in the	======
  * ====== specified group or MPI_UNDEFINED if the	======
  * ====== calling process is not a member of the	======
  * ====== group.					======
  * ======						======
  * =========================================================
  */


 /* =========================================================
  * ======  This is the group "group_worker".		======
  * =========================================================
  */
 MPI_Group_rank (group_worker, &group_w_mytid);
 if (group_w_mytid != MPI_UNDEFINED)
 {
   MPI_Comm_size (COMM_WORKER, &group_w_ntasks);  /* # of processes	*/
   /* Now let's start with the real work				*/
   MPI_Get_processor_name (processor_name, &namelen);
   /* With the next statement every process executing this code will
    * print one line on the display. It may happen that the lines will
    * get mixed up because the display is a critical section. In general
    * only one process (mostly the process with rank 0) will print on
    * the display and all other processes will send their messages to
    * this process. Nevertheless for debugging purposes (or to
    * demonstrate that it is possible) it may be useful if every
    * process prints itself.
    */
   fprintf (stdout, "Process %d of %d running on %s\n",
	     group_w_mytid, group_w_ntasks, processor_name);
   fflush (stdout);
   MPI_Barrier (COMM_WORKER);		/* wait for all other processes	*/

   /* Build the new type for a strided vector and resize the extent
    * of the new datatype in such a way that the extent of the whole
    * column looks like just one element so that the next column
    * starts in matrix[0][i] in MPI_Scatterv/MPI_Gatherv.
    */
   MPI_Type_vector (P, 1, Q, MPI_DOUBLE, &tmp_column_t);
   MPI_Type_create_resized (tmp_column_t, 0, sizeof (double),
			     &column_t);
   MPI_Type_commit (&column_t);
   MPI_Type_free (&tmp_column_t);
   if (group_w_mytid == 0)
   {
     tmp = 1;
     for (i = 0; i < P; ++i)		/* initialize matrix		*/
     {
	for (j = 0; j < Q; ++j)
       {
	  matrix[i][j] = tmp++;
	}
     }
     printf ("\n\noriginal matrix:\n\n");
     print_matrix (P, Q, (double **) matrix);
   }
   /* allocate memory for array containing the number of columns of a
    * column block for each process
    */
   num_columns = (int *) malloc (group_w_ntasks * sizeof (int));
   TestEqualsNULL (num_columns);	/* test if memory was available	*/

   /* do an unnecessary initialization to make the GNU compiler happy
    * so that you won't get a warning about the use of a possibly
    * uninitialized variable
    */
   sr_counts = NULL;
   sr_disps  = NULL;
   if (group_w_mytid == 0)
   {
     /* allocate memory for arrays containing the size and
      * displacement of each column block
      */
     sr_counts = (int *) malloc (group_w_ntasks * sizeof (int));
     TestEqualsNULL (sr_counts);
     sr_disps = (int *) malloc (group_w_ntasks * sizeof (int));
     TestEqualsNULL (sr_disps);
     /* compute number of columns in column block for each process	*/
     tmp = Q / group_w_ntasks;
     for (i = 0; i < group_w_ntasks; ++i)
     {
	num_columns[i] = tmp;		/* number of columns		*/
     }
     for (i = 0; i < (Q % group_w_ntasks); ++i)	/* adjust size 	*/
     {
	num_columns[i]++;
     }
     for (i = 0; i < group_w_ntasks; ++i)
     {
	/* nothing to do because "column_t" contains already all
	 * elements of a column, i.e., the "size" is equal to the
	 * number of columns in the block
	 */
	sr_counts[i] = num_columns[i];	/* "size" of column-block	*/
     }
     sr_disps[0] = 0;			/* start of i-th column-block	*/
     for (i = 1; i < group_w_ntasks; ++i)
     {
	sr_disps[i] = sr_disps[i - 1] + sr_counts[i - 1];
     }
   }
   /* inform all processes about their column block sizes		*/
   MPI_Bcast (num_columns, group_w_ntasks, MPI_INT, 0, COMM_WORKER);
   /* allocate memory for a column block and define a new derived
    * data type for the column block. This data type is possibly
    * different for different processes if the number of processes
    * isn't a factor of the row size of the original matrix. Don't
    * forget to resize the extent of the new data type in such a
    * way that the extent of the whole column looks like just one
    * element so that the next column starts in col_block[0][i]
    * in MPI_Scatterv/MPI_Gatherv.
    */
   col_block = (double **) malloc (P * num_columns[group_w_mytid] *
				    sizeof (double));
   TestEqualsNULL (col_block);
   MPI_Type_vector (P, 1, num_columns[group_w_mytid], MPI_DOUBLE,
		     &tmp_column_t);
   MPI_Type_create_resized (tmp_column_t, 0, sizeof (double),
			     &col_block_t);
   MPI_Type_commit (&col_block_t);
   MPI_Type_free (&tmp_column_t);
   /* send column block i of "matrix" to process i			*/
   MPI_Scatterv (matrix, sr_counts, sr_disps, column_t,
		  col_block, num_columns[group_w_mytid],
		  col_block_t, 0, COMM_WORKER);
   /* Modify column elements. The compiler doesn't know the structure
    * of the column block matrix so that you have to do the index
    * calculations for mat[i][j] yourself. In C a matrix is stored
    * row-by-row so that the i-th row starts at location "i * q" if
    * the matrix has "q" columns. Therefore the address of mat[i][j]
    * can be expressed as "(double *) mat + i * q + j" and mat[i][j]
    * itself as "*((double *) mat + i * q + j)".
    */
   for (i = 0; i < P; ++i)
   {
     for (j = 0; j < num_columns[group_w_mytid]; ++j)
     {
	if ((group_w_mytid % 2) == 0)
	{
	  /* col_block[i][j] *= col_block[i][j]				*/

	  *((double *) col_block + i * num_columns[group_w_mytid] + j) *=
	  *((double *) col_block + i * num_columns[group_w_mytid] + j);
	}
	else
	{
	  /* col_block[i][j] *= FACTOR					*/

	  *((double *) col_block + i * num_columns[group_w_mytid] + j) *=
	    FACTOR;
	}
     }
   }
   /* receive column-block i of "matrix" from process i		*/
   MPI_Gatherv (col_block, num_columns[group_w_mytid], col_block_t,
		 matrix, sr_counts, sr_disps, column_t,
		 0, COMM_WORKER);
   if (group_w_mytid == 0)
   {
     printf ("\n\nresult matrix:\n"
	      "  elements are sqared in columns:\n  ");
     tmp  = 0;
     tmp1 = 0;
     for (i = 0; i < group_w_ntasks; ++i)
     {
	tmp1 = tmp1 + num_columns[i];
	if ((i % 2) == 0)
	{
	  for (j = tmp; j < tmp1; ++j)
	  {
	    printf ("%4d", j);
	  }
	}
	tmp = tmp1;
     }
     printf ("\n  elements are multiplied with %d in columns:\n  ",
	      FACTOR);
     tmp  = 0;
     tmp1 = 0;
     for (i = 0; i < group_w_ntasks; ++i)
     {
	tmp1 = tmp1 + num_columns[i];
	if ((i % 2) != 0)
	{
	  for (j = tmp; j < tmp1; ++j)
	  {
	    printf ("%4d", j);
	  }
	}
	tmp = tmp1;
     }
     printf ("\n\n\n");
     print_matrix (P, Q, (double **) matrix);
     free (sr_counts);
     free (sr_disps);
   }
   free (num_columns);
   free (col_block);
   MPI_Type_free (&column_t);
   MPI_Type_free (&col_block_t);
   MPI_Comm_free (&COMM_WORKER);
 }


 /* =========================================================
  * ======  This is the group "group_other".		======
  * =========================================================
  */
 MPI_Group_rank (group_other, &group_o_mytid);
 if (group_o_mytid != MPI_UNDEFINED)
 {
   /* Nothing to do (only to demonstrate how to divide work for
    * different groups).
    */
   MPI_Comm_size (COMM_OTHER, &group_o_ntasks);
   if (group_o_mytid == 0)
   {
     if (group_o_ntasks == 1)
     {
	printf ("\nGroup \"group_other\" contains %d process "
		"which has\n"
		"nothing to do.\n\n", group_o_ntasks);
     }
     else
     {
	printf ("\nGroup \"group_other\" contains %d processes "
		"which have\n"
		"nothing to do.\n\n", group_o_ntasks);
     }
   }
   MPI_Comm_free (&COMM_OTHER);
 }


 /* =========================================================
  * ======  all groups will reach this point		======
  * =========================================================
  */
 MPI_Group_free (&group_worker);
 MPI_Group_free (&group_other);
 MPI_Finalize ();
 return EXIT_SUCCESS;
}


/* Print the values of an arbitrary 2D-matrix of "double" values. The
* compiler doesn't know the structure of the matrix so that you have
* to do the index calculations for mat[i][j] yourself. In C a matrix
* is stored row-by-row so that the i-th row starts at location "i * q"
* if the matrix has "q" columns. Therefore the address of mat[i][j]
* can be expressed as "(double *) mat + i * q + j" and mat[i][j]
* itself as "*((double *) mat + i * q + j)".
*
* input parameters:	p	number of rows
*			q	number of columns
*			mat	2D-matrix of "double" values
* output parameters:	none
* return value:	none
* side effects:	none
*
*/
void print_matrix (int p, int q, double **mat)
{
 int i, j;				/* loop variables		*/

 for (i = 0; i < p; ++i)
 {
   for (j = 0; j < q; ++j)
   {
     printf ("%6g", *((double *) mat + i * q + j));
   }
   printf ("\n");
 }
 printf ("\n");
}


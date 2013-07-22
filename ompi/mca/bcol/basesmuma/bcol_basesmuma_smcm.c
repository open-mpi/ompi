/*
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include "ompi/proc/proc.h"
#include "ompi/patterns/comm/coll_ops.h"

#include "opal/dss/dss.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/align.h"

#include "bcol_basesmuma.h"



#define SM_BACKING_FILE_NAME_MAX_LEN 256

struct file_info_t {
    uint32_t vpid;
    uint32_t jobid;
    uint64_t file_size;
    uint64_t size_ctl_structure;
    uint64_t data_seg_alignment;
    char file_name[SM_BACKING_FILE_NAME_MAX_LEN];
};

/* need to allocate space for the peer */
static void bcol_basesmuma_smcm_proc_item_t_construct
(bcol_basesmuma_smcm_proc_item_t * item) {
}

/* need to free the space for the peer */
static void bcol_basesmuma_smcm_proc_item_t_destruct
    (bcol_basesmuma_smcm_proc_item_t * item) {
}

OBJ_CLASS_INSTANCE(bcol_basesmuma_smcm_proc_item_t,
        opal_list_item_t,
        bcol_basesmuma_smcm_proc_item_t_construct, 
        bcol_basesmuma_smcm_proc_item_t_destruct);




bcol_basesmuma_smcm_mmap_t* bcol_basesmuma_smcm_create_mmap(int fd, size_t size, char *file_name,
        size_t size_ctl_structure,
        size_t data_seg_alignment)
{
    bcol_basesmuma_smcm_mmap_t *map;
    bcol_basesmuma_smcm_file_header_t *seg;
    unsigned char *addr = NULL;

    /* map the file and initialize segment state */
    seg = (bcol_basesmuma_smcm_file_header_t*)
        mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if((void*)-1 == seg) {
        return NULL;
    }

    /* set up the map object */
    map = (bcol_basesmuma_smcm_mmap_t* )malloc(sizeof(bcol_basesmuma_smcm_mmap_t));
    assert(map);

    strncpy(map->map_path, file_name, OPAL_PATH_MAX);
    /* the first entry in the file is the control structure. The first
       entry in the control structure is an mca_common_sm_file_header_t
       element */
    map->map_seg = seg;

    addr = ((unsigned char *)seg) + size_ctl_structure;
    /* If we have a data segment (i.e., if 0 != data_seg_alignment),
       then make it the first aligned address after the control
       structure. */
    if (0 != data_seg_alignment) {
        addr = OPAL_ALIGN_PTR(addr,  data_seg_alignment, unsigned char*);

        /* is addr past end of file ? */
        if((unsigned char*)seg + size < addr) {
            opal_output(0, "bcol_basesmuma_smcm_mmap_init: "
                    "memory region too small len %lu  addr %p\n",
                    (unsigned long)size, addr);
            return NULL;
        }
    }
    map->data_addr = addr;
    map->map_addr = (unsigned char *)seg;
    map->map_size = size;

    return map;
}




/* smcm_allgather_connection:  
   This function is called when a shared memory subgroup wants to establish shared memory "connections" amongs
   a group of processes. 
   
   This function DOES NOT create any shared memory backing files, it only mmaps already existing files. Shared
   memory files are created by the shared memory registration function 
   -----------------------------------------------------------------------------------------------------------
   Input params:

   - sbgp module   The subgrouping module contains the list of ranks to wire up.

   - peer_list      An opal list containing a list of bcol_basesmuma_smcm_proc_item_t types. This
   contains a list of peers whose shared memory files I have already mapped. 
   Upon completion of the allgather exchange with all members of the group and depending on the
   value of "map_all", my peers' shared memory files are mapped into my local virtual memory
   space, with all pertinent information being stored in an bcol_basesmuma_smcm_proc_item_t which is 
   subsequently appended onto the "peer_list". 

   - comm           The ompi_communicator_t communicator.  

   - input          A data struct that caches the information about my shared memory file. 
    
   - map_all        Bool that determines whether or not to go ahead and map the files from all of the peers 
   defined in the sbgp-ing module. If map_all == true, then go ahead and mmap all of the files
   obtained in the exchange and append the information to the "peer_list". If map_all == false
   then make a check and only mmap those peers' files whose vpid/jobid/filename combination do 
   not already exist in the "peer_list". Once mapping is completed, append this peer's information 
   to the "peer_list".
   -----------------------------------------------------------------------------------------------------------
 *
 */


int bcol_basesmuma_smcm_allgather_connection(
        mca_bcol_basesmuma_module_t *sm_bcol_module,
        mca_sbgp_base_module_t *module,
        opal_list_t *peer_list,
        bcol_basesmuma_smcm_proc_item_t ***back_files,	
        ompi_communicator_t *comm,
        bcol_basesmuma_smcm_file_t input,
        char *base_fname,
        bool map_all)
{

	/* define local variables */

	int rc, i, cnt, index_in_group, fd = -1, n_files_mapped;
	size_t len, len_other;
	uint32_t rem_vpid, rem_jobid;
	uint64_t rem_size, rem_size_ctl_struct, rem_data_seg_align;
	char *rem_fname,*cpy_ret;
	ptrdiff_t mem_offset;
	ompi_proc_t *proc_temp, *my_id;
	bcol_basesmuma_smcm_proc_item_t *temp;
	bcol_basesmuma_smcm_proc_item_t *item_ptr = OBJ_NEW(bcol_basesmuma_smcm_proc_item_t);
	bcol_basesmuma_smcm_proc_item_t **backing_files;
        struct file_info_t local_file;
        struct file_info_t *all_files=NULL;

	backing_files= (bcol_basesmuma_smcm_proc_item_t **)
        malloc(sizeof(bcol_basesmuma_smcm_proc_item_t *)*module->group_size);
	if( !backing_files ) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }
        *back_files=backing_files;

        /* check to see if we have already mapped all the files, if we have
         * just need to fill in backing_files array, and we are done
         */
        n_files_mapped=0;
        for (i = 0; i < module->group_size; i++) {
            /* get the proc info */
            proc_temp = ompi_comm_peer_lookup(comm,module->group_list[i]);
            rem_jobid = proc_temp->proc_name.jobid;
            rem_vpid = proc_temp->proc_name.vpid;
            index_in_group=i;

            for (item_ptr = (bcol_basesmuma_smcm_proc_item_t*) opal_list_get_first(peer_list); 
                    item_ptr != (bcol_basesmuma_smcm_proc_item_t*) opal_list_get_end(peer_list); 
                    item_ptr = (bcol_basesmuma_smcm_proc_item_t*) opal_list_get_next((opal_list_item_t *)item_ptr)) {

                /* if the vpid/jobid/filename combination already exists in the list,
                   then do not map this peer's file --- because you already have */
                if (rem_vpid == item_ptr->peer.vpid && rem_jobid == item_ptr->peer.jobid
                        && (strstr(item_ptr->sm_file.file_name,base_fname)) ){

                    /* record file data */
                    /* RLG - note - is this correct ? */
                    /*sm_bcol_module->ctl_backing_files_info[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *)item_ptr; */
                    backing_files[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *)item_ptr;
                    n_files_mapped++;
                    /* found it - no need to continue looking */
                    break;
                }	
            }
        }
        /* check to see if we are done - our own files are not in this list*/
        if (n_files_mapped == (module->group_size-1) ) {
            return OMPI_SUCCESS;
        }


	/* Phase One: 
	   gather a list of processes that will participate in the allgather - I'm 
	   preparing this list from the sbgp-ing module that was passed into the function */

        my_id = ompi_proc_local();

        /* fill in local file information */
        local_file.vpid=my_id->proc_name.vpid;
        local_file.jobid=my_id->proc_name.jobid;
        local_file.file_size=input.size;
        local_file.size_ctl_structure=input.size_ctl_structure;
        local_file.data_seg_alignment=input.data_seg_alignment;

        len=strlen(input.file_name);
        if( len > SM_BACKING_FILE_NAME_MAX_LEN-1 ) {
            fprintf(stderr," backing file name too long:  %s len :: %d \n",
                    input.file_name,(int) len);
		rc = OMPI_ERROR;
		goto Error;
        }
        strcpy(&(local_file.file_name[0]),input.file_name);
        local_file.file_name[len]='\0';

        /* will exchange this data type as a string of characters -
         * this routine is first called before MPI_init() completes
         * and before error handling is setup, so can't use the
         * MPI data types to send this data */
        len=sizeof(struct file_info_t);
        all_files=(struct file_info_t *)malloc(
                module->group_size*len);
        if( !all_files ) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }

        /* initialize the destination array */
        bzero(all_files,sizeof(struct file_info_t)*
                sm_bcol_module->super.sbgp_partner_module->group_size);

        /* exchange data */
        rc=comm_allgather_pml(&local_file,all_files,sizeof(struct file_info_t),
                MPI_CHAR,
                sm_bcol_module->super.sbgp_partner_module->my_index,
                sm_bcol_module->super.sbgp_partner_module->group_size,
                sm_bcol_module->super.sbgp_partner_module->group_list,
                sm_bcol_module->super.sbgp_partner_module->group_comm);
        if( OMPI_SUCCESS != rc ) {
            fprintf(stderr," failed in comm_allgather_pml.  Error code: %d \n",
                    rc);
            fflush(stderr);
            goto Error;
        }

	/* Phase four: 
	   loop through the receive buffer, unpack the data recieved from remote peers */

        for (i = 0; i < module->group_size; i++) {

            index_in_group=i;
            rem_vpid=all_files[i].vpid;
            rem_jobid=all_files[i].jobid;
            rem_fname=&(all_files[i].file_name[0]);
            rem_size=all_files[i].file_size;
            rem_size_ctl_struct=all_files[i].size_ctl_structure;
            rem_data_seg_align=all_files[i].data_seg_alignment;

            /* if this is me, release resources and continue */
            len=strlen(input.file_name);
            len_other=strlen(rem_fname);
            if( (rem_vpid == my_id->proc_name.vpid) &&
                    (rem_jobid == my_id->proc_name.jobid) &&
                    ( len == len_other ) &&
                    (strncmp(input.file_name,rem_fname, len) == 0 )
              ) {

                /*free(rem_fname); */
                continue;
            }

            temp = OBJ_NEW(bcol_basesmuma_smcm_proc_item_t);

            temp->peer.vpid = rem_vpid;
            temp->peer.jobid = rem_jobid;
            temp->sm_file.file_name = (char *) malloc(len_other+1);
            if( !temp->sm_file.file_name) {
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto Error;
            }
            cpy_ret=strncpy(temp->sm_file.file_name,&(all_files[i].file_name[0]),
                    len_other);
            if( !cpy_ret ) {
                rc = OMPI_ERROR;
                goto Error;
            }
            temp->sm_file.file_name[len_other]='\0';

            temp->sm_file.size = (size_t) rem_size;
            temp->sm_file.mpool_size = (size_t) rem_size;
            temp->sm_file.size_ctl_structure = (size_t) rem_size_ctl_struct;
            temp->sm_file.data_seg_alignment = (size_t) rem_data_seg_align;

            /* Phase Five:
               If map_all == true, then  we map every peer's file
               else we check to see if I have already mapped this 
               vpid/jobid/filename combination and if I have, then
               I do not mmap this peer's file.
             * 
             */
            if(map_all) {
                fd = -1;
                fd = open(temp->sm_file.file_name,O_RDWR,0600);
                if(0 > fd) {
                    fprintf(stderr,"SMCM Allgather failed to open sm backing file \n");
                    fflush(stderr);
                    rc = OMPI_ERROR;
                    goto Error;
                } else {

                    /* map the file */
                    temp->sm_mmap = bcol_basesmuma_smcm_create_mmap(fd,temp->sm_file.size, 
                            temp->sm_file.file_name,
                            temp->sm_file.size_ctl_structure,
                            getpagesize());
                    if (NULL == temp->sm_mmap) {
                        fprintf(stderr,"mmapping failed to map remote peer's file\n");
                        fflush(stderr);
                        rc = OMPI_ERROR;
                        goto Error;
                    }

                    /* compute memory offset */
                    mem_offset = (ptrdiff_t) temp->sm_mmap->data_addr -
                        (ptrdiff_t) temp->sm_mmap->map_seg;
                    temp->sm_mmap->map_seg->seg_offset = mem_offset;
                    temp->sm_mmap->map_seg->seg_size = temp->sm_file.size - mem_offset;
                    /* more stuff to follow */
                }

                /* append this peer's info, including shared memory map addr, onto the
                   peer_list */

                /* record file data */
                sm_bcol_module->ctl_backing_files_info[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *)temp;
                backing_files[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *) temp;

                opal_list_append(peer_list, (opal_list_item_t*) temp);

            } else {
                /* check to see if I have already mapped this peer's file */
                cnt = 0;
                for (item_ptr = (bcol_basesmuma_smcm_proc_item_t*) opal_list_get_first(peer_list); 
                        item_ptr != (bcol_basesmuma_smcm_proc_item_t*) opal_list_get_end(peer_list); 
                        item_ptr = (bcol_basesmuma_smcm_proc_item_t*) opal_list_get_next((opal_list_item_t *)item_ptr)) {

                    /* if the vpid/jobid/filename combination already exists in the list,
                       then do not map this peer's file --- because you already have */
                    if (rem_vpid == item_ptr->peer.vpid && rem_jobid == item_ptr->peer.jobid
                            && (0 == strcmp(rem_fname,item_ptr->sm_file.file_name)) ){
                        cnt++;

                        /* record file data */
                        sm_bcol_module->ctl_backing_files_info[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *)item_ptr;
                        backing_files[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *)item_ptr;
                        /* found it - no need to continue looking */
                        break;
                    }	
                }

                if (cnt == 0) {

                    /* then we haven't mmapped this file, so let's do it now. watch out josh,
                       the ordering of procs may sting you! Do we need to pack in a rank? */

                    fd = -1;
                    fd = open(temp->sm_file.file_name,O_RDWR,0600);
                    if (0 > fd) {
                        fprintf(stderr,"file open failed %s \n",
                                temp->sm_file.file_name);
                        fflush(stderr);
                        rc = OMPI_ERROR;
                        goto Error;
                    } else {

                        /* map this file */

                        temp->sm_mmap = bcol_basesmuma_smcm_create_mmap(fd,temp->sm_file.size,
                                temp->sm_file.file_name, 
                                temp->sm_file.size_ctl_structure,
                                getpagesize());
                        if (NULL == temp->sm_mmap) {
                            fprintf(stderr,"mmapping failed to map remote peer's file\n");
                            fflush(stderr);
                            rc = OMPI_ERROR;
                            goto Error;
                        }

                        /* initialize the segment */
                        mem_offset = (ptrdiff_t) temp->sm_mmap->data_addr -
                            (ptrdiff_t) temp->sm_mmap->map_seg;

                    }			

                    /* record file data */
                    backing_files[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *) temp;
                    /*sm_bcol_module->ctl_backing_files_info[index_in_group]=(bcol_basesmuma_smcm_proc_item_t *)temp;*/
                    opal_list_append(peer_list, (opal_list_item_t*) temp);

                } else {
                    /* free the allocated memory - we are not going to put this on any list */
                    OBJ_RELEASE(temp);
                }
            }

        }

	/* clean-up */
        if( NULL != all_files ) {
            free(all_files);
            all_files=NULL;
        }

	return OMPI_SUCCESS;


Error:

	/* error clean-up and return */
        if( NULL != all_files ) {
            free(all_files);
            all_files=NULL;
        }

	return rc;
}



OBJ_CLASS_INSTANCE(
        bcol_basesmuma_smcm_mmap_t,
        opal_list_item_t,
        NULL,
        NULL
        );



/*
 * mmap the specified file as a shared file.  No information exchange with other
 * processes takes place within this routine.
 * This function assumes that the memory has already been allocated, and only the
 * mmap needs to be done.
 */
bcol_basesmuma_smcm_mmap_t *bcol_basesmuma_smcm_mem_reg(void *in_ptr,
		size_t length,
		size_t alignment,
		char* file_name)
{
	/* local variables */
	int fd = -1;
	bcol_basesmuma_smcm_mmap_t *map = NULL;
        /* if pointer is not allocated - return error.  We have no clue how the user will allocate or
         *   free this memory.
         */

	/* open the shared memory backing file */
	
	fd = open(file_name, O_CREAT|O_RDWR,0600);
	if (fd < 0) {
		opal_output(0, "basesmuma shared memory allocation open failed with errno: %d\n",
					errno);
	} else if (0 != ftruncate(fd,length)) {
		opal_output(0, "basesmuma shared memory allocation ftruncate failed with errno: %d\n",
					errno);
	} else {
		
		map = bcol_basesmuma_smcm_reg_mmap(in_ptr, fd, length, alignment, file_name);
		if (NULL == map) {
			return NULL;
		}
	}
	
	/* takes us to the top of the control structure */
	
	return map;
	
}

bcol_basesmuma_smcm_mmap_t * bcol_basesmuma_smcm_reg_mmap(void *in_ptr,
		int fd,
		size_t length, 
		size_t alignment,
		char *file_name)
{
	
	/* local variables */
	bcol_basesmuma_smcm_mmap_t *map;
	bcol_basesmuma_smcm_file_header_t *seg;
	unsigned char* myaddr = NULL;
	
	/* map the file and initialize the segment state */
	seg = (bcol_basesmuma_smcm_file_header_t *) 
            mmap(in_ptr, length, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, fd, 0);
	if((void*)-1 == seg) {
		return NULL;
	}
	
	/* set up the map object */
	
	/*map = OBJ_NEW(mca_common_sm_mmap_t); */
        map=(bcol_basesmuma_smcm_mmap_t *)malloc(sizeof(bcol_basesmuma_smcm_mmap_t));
        assert(map);
	strncpy(map->map_path, file_name, OPAL_PATH_MAX);
	
	/* the first entry in the file is the control structure. the first entry 
	 in the control structure is an mca_common_sm_file_header_t element */
	map->map_seg = seg;
	
	myaddr = (unsigned char *) seg;
	/* if we have a data segment (i.e. if 0 != data_seg_alignement) */
	
	if ( 0 != alignment) {
		myaddr = OPAL_ALIGN_PTR(myaddr, alignment, unsigned char*);
		
		/* is addr past the end of the file? */
		if ((unsigned char *) seg+length < myaddr) {
			opal_output(0, "mca_bcol_basesmuma_sm_alloc_mmap: memory region too small len %lu add %p\n",
						(unsigned long) length, myaddr);
			return NULL;
		}
		
	}	
	
	
	map->data_addr = (unsigned char*) myaddr;
	map->map_addr = (unsigned char*) seg;
	map->map_size = length;

	return map;
	
}



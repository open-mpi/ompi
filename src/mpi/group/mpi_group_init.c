/*
 * $HEADER$
 */

#include "mpi/group/group.h"
#include "lam/constants.h"

/* define class information */
static void lam_group_construct(lam_group_t *);
static void lam_group_destruct(lam_group_t *);

lam_class_t lam_group_t_class = {
    "lam_group_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t) lam_group_construct,
    (lam_destruct_t) lam_group_destruct
};

/*
 * Table for Fortran <-> C group handle conversion
 */
lam_pointer_array_t *lam_group_f_to_c_table;

/**
 * This routine is used to allocate a new group structure
 *
 * @return Pointer to new group structure
 */
lam_group_t *group_allocate(int group_size)
{
    /* local variables */
    lam_group_t *new_group;
    int ret_val;

    /* create new group group element */
    new_group=OBJ_NEW(lam_group_t);

    if( new_group ) {
        /* allocate array of (lam_proc_t *)'s, one for each
         *   process in the group */
        new_group->grp_proc_pointers=
            malloc(sizeof(lam_proc_t *)*group_size);
        if( new_group->grp_proc_pointers ) {
            /* grp_proc_pointers allocated */
            new_group->grp_proc_count=group_size;
            /* assign entry in fortran <-> c translation array */
            ret_val=lam_pointer_array_add(lam_group_f_to_c_table,new_group);
            if( -1 == ret_val ){
                OBJ_RELEASE(new_group);
                new_group=NULL;
            }
            new_group->grp_f_to_c_index=ret_val;
        } else {
            /* grp_proc_pointers allocation failed */
            free(new_group);
            new_group=NULL;
        }
    }

    /* return */
    return new_group;
}

/**
 * group constructor
 */
void lam_group_construct(lam_group_t *new_group){

    /* return */
    return;
}

/**
 * group destructor
 */
void lam_group_destruct(lam_group_t *group){

    int return_value;

    /* release thegrp_proc_pointers memory */
    if( NULL != group->grp_proc_pointers )
        free(group->grp_proc_pointers);

    /* reset the lam_group_f_to_c_table entry - make sure that the
     * entry is in the table */
    if( NULL!= lam_pointer_array_get_item(lam_group_f_to_c_table,
                group->grp_f_to_c_index) ) {
        lam_pointer_array_set_item(lam_group_f_to_c_table,
                group->grp_f_to_c_index, NULL);
    }

    /* return */
    return;
}

/**
 * Initialize LAM group infrastructure
 */

int lam_group_init(void)
{
    /* local variables */
    int return_value=LAM_SUCCESS;

    /* initialize lam_group_f_to_c_table */
    lam_group_f_to_c_table=OBJ_NEW(lam_pointer_array_t);
    if( NULL == lam_group_f_to_c_table )
        return_value=LAM_ERROR;

    /* return */
    return return_value;
}

/**
 * Clean up group infrastructure
 */
int lam_group_finalize(void){
    /* local variables */
    int return_value=LAM_SUCCESS;

    OBJ_RELEASE(lam_group_f_to_c_table);

    /* return */
    return return_value;
};

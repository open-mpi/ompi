/*
 * $HEADER$
 */

/** @file datatype creation function */

#include "lam_config.h"
#include "lam/constants.h"
#include "mpi.h"
#include "datatype.h"

/**
 * Create a LAM/MPI datatype
 *
 * @param combiner   integer identifying the kind of MPI create function
 * @param ninteger   number of integers passed to the create function
 * @param integer    array of integers passed to the create function
 * @param naddress   number of addresses passed to the create function
 * @param address    array of addresses passed to the create function
 * @param ntype      number of data types passed to the create function
 * @param type       array of data types passed to the create function
 * @param newtype    pointer to address of new type
 * @return           LAM_SUCCESS on successful creation, LAM_ERROR otherwise
 *
 * This is the central location for creation of data types in LAM/MPI.
 * All MPI_Type_create functions rely upon this to do the actual type
 * creation.
 */
int lam_datatype_create(int combiner,
                        int nintegers,
                        int integers[],
                        int naddresses,
                        ssize_t addresses[],
                        int ntypes,
                        lam_datatype_t *types[],
                        lam_datatype_t **newtype)
{
#if 0
    lam_datatype_t *newtype, *t;
    lam_datatype_t **types = (lam_datatype_t **) array_of_types;
    int i, j, k;
    int mpi_lb_i, mpi_ub_i;
    int min_lb, max_ub;
    int min_disp_i, max_disp_i;
    int typemap_i;
    ssize_t lb, ub, min_disp, max_disp, typemap_off, current_offset,
        new_offset;
    size_t current_size, new_size;
    int rc;

    if (newdatatype == NULL) {
        ulm_err(("Error: MPI_Type_struct: Invalid newtype pointer\n"));
        rc = MPI_ERR_TYPE;
        _mpi_errhandler(MPI_COMM_WORLD, rc, __FILE__, __LINE__);
        return rc;
    }

    if (count < 0) {
        ulm_err(("Error: MPI_Type_struct: count %d is invalid\n", count));
        rc = MPI_ERR_INTERN;
        _mpi_errhandler(MPI_COMM_WORLD, rc, __FILE__, __LINE__);
        return rc;
    }

    if (count == 0) {
        newtype = (lam_datatype_t *) LAM_MALLOC(sizeof(lam_datatype_t));
        if (newtype == NULL) {
            ulm_err(("Error: MPI_Type_struct: Out of memory\n"));
            rc = MPI_ERR_TYPE;
            _mpi_errhandler(MPI_COMM_WORLD, rc, __FILE__, __LINE__);
            return rc;
        }

        newtype->isbasic = 0;
        newtype->layout = CONTIGUOUS;
        newtype->num_pairs = 0;
        newtype->extent = 0;
        newtype->lower_bound = 0;
        newtype->type_map = NULL;
        newtype->committed = 0;
        newtype->ref_count = 1;

        *newdatatype = newtype;

        /* save "envelope" information */
        newtype->envelope.combiner = MPI_COMBINER_STRUCT;
        newtype->envelope.nints = 1;
        newtype->envelope.naddrs = 0;
        newtype->envelope.ndatatypes = 0;
        newtype->envelope.iarray = (int *) LAM_MALLOC(sizeof(int));
        newtype->envelope.aarray = NULL;
        newtype->envelope.darray = NULL;
        newtype->envelope.iarray[0] = count;

        if (_mpi.fortran_layer_enabled) {
            newtype->fhandle = _mpi_ptr_table_add(_mpif.type_table, newtype);
        }

        return MPI_SUCCESS;
    }

    /* Allocate new type */
    newtype = LAM_MALLOC(sizeof(lam_datatype_t));
    if (newtype == NULL) {
        ulm_err(("Error: MPI_Type_struct: Out of memory\n"));
        rc = MPI_ERR_TYPE;
        _mpi_errhandler(MPI_COMM_WORLD, rc, __FILE__, __LINE__);
        return rc;
    }

    /* Initialize newtype */
    newtype->isbasic = 0;
    newtype->op_index = 0;
    newtype->layout = NON_CONTIGUOUS;
    newtype->committed = 0;
    newtype->ref_count = 1;

    /* save "envelope" information */
    newtype->envelope.combiner = MPI_COMBINER_STRUCT;
    newtype->envelope.nints = count + 1;
    newtype->envelope.naddrs = count;
    newtype->envelope.ndatatypes = count;
    newtype->envelope.iarray =
        (int *) LAM_MALLOC(newtype->envelope.nints * sizeof(int));
    newtype->envelope.aarray =
        (MPI_Aint *) LAM_MALLOC(newtype->envelope.naddrs *
                                sizeof(MPI_Aint));
    newtype->envelope.darray =
        (MPI_Datatype *) LAM_MALLOC(newtype->envelope.ndatatypes *
                                    sizeof(MPI_Datatype));
    newtype->envelope.iarray[0] = count;
    for (i = 0; i < count; i++) {
        newtype->envelope.iarray[i + 1] = array_of_blocklengths[i];
        newtype->envelope.aarray[i] = array_of_displacements[i];
        newtype->envelope.darray[i] = array_of_types[i];
        t = array_of_types[i];
        fetchNadd(&(t->ref_count), 1);
    }

    /*
     * Look for MPI_LB, MPI_UB markers, smallest/largest
     * displacements, and save off indices
     */
    mpi_lb_i = -1;
    mpi_ub_i = -1;
    /* initialize min_lb and max_lb to quiet not-so-bright compilers */
    min_lb = 0;
    max_ub = 0;
    min_disp = array_of_displacements[0];
    max_disp = array_of_displacements[0];
    min_disp_i = 0;
    max_disp_i = 0;
    for (i = 0; i < count; i++) {
        if (types[i]->extent == 0) {
            if (types[i]->op_index == -1) {
                if ((mpi_lb_i == -1)
                    || (array_of_displacements[i] < min_lb)) {
                    min_lb = array_of_displacements[i];
                    mpi_lb_i = i;
                }
            } else if (types[i]->op_index == -2) {
                if ((mpi_lb_i == -1)
                    || (array_of_displacements[i] > max_ub)) {
                    max_ub = array_of_displacements[i];
                    mpi_ub_i = i;
                }
            }
        }
        if (types[i]->lower_bound > 0) {
            if ((mpi_lb_i == -1) || (types[i]->lower_bound < min_lb)) {
                min_lb = types[i]->lower_bound;
                mpi_lb_i = i;
            }
        }
        if (array_of_displacements[i] < min_disp) {
            min_disp = array_of_displacements[i];
            min_disp_i = i;
        }
        if (array_of_displacements[i] > max_disp) {
            max_disp = array_of_displacements[i];
            max_disp_i = i;
        }
    }

    /*
     * calculate the new datatype's extent, and set the
     * lower bound
     */
    lb = 0, ub = 0;
    if (mpi_lb_i != -1) {
        lb = min_lb;
    } else {
        lb = array_of_displacements[min_disp_i];
    }
    if (mpi_ub_i != -1) {
        ub = max_ub;
    } else {
        ub = array_of_displacements[max_disp_i]
            + array_of_blocklengths[max_disp_i]
            * types[max_disp_i]->extent;
    }
    /* extent should never be less than zero */
    if (ub < lb) {
        ub = lb;
    }
    newtype->extent = ub - lb;
    newtype->lower_bound = lb;

    /* calculate the number of entries needed for the new type_map */
    typemap_i = 0;
    current_size = current_offset = 0;
    for (i = 0; i < count; i++) {
        if (types[i]->extent > 0) {
            typemap_off = array_of_displacements[i];
            for (j = 0; j < array_of_blocklengths[i]; j++) {
                for (k = 0; k < types[i]->num_pairs; k++) {
                    new_size = types[i]->type_map[k].size;
                    new_offset = types[i]->type_map[k].offset +
                        typemap_off;
                    if ((typemap_i != 0)
                        && (current_size + current_offset == new_offset)) {
                        /* consolidate entries */
                        current_size += new_size;
                        if (_MPI_DTYPE_TRIM
                            && current_size + current_offset > ub) {
                            current_size = ub - current_offset;
                        }
                    } else {
                        if (!_MPI_DTYPE_TRIM
                            || ((new_offset + new_size > lb)
                                && (new_offset < ub))) {
                            /* create new type_map entry if there is still something 
                             * left after possible trimming */
                            if (_MPI_DTYPE_TRIM && new_offset < lb) {
                                new_size -= (lb - new_offset);
                                new_offset = lb;
                            }
                            if (_MPI_DTYPE_TRIM
                                && new_offset + new_size > ub) {
                                new_size = (ub - new_offset);
                            }
                            if (new_size > 0) {
                                current_size = new_size;
                                current_offset = new_offset;
                                typemap_i++;
                            }
                        }
                    }
                }
                typemap_off += types[i]->extent;
            }
        }
    }
    /* more newtype initialization */
    newtype->num_pairs = typemap_i;
    if (newtype->num_pairs > 0) {
        /* allocate the type_map */
        newtype->type_map = (ULMTypeMapElt_t *)
            LAM_MALLOC(newtype->num_pairs * sizeof(ULMTypeMapElt_t));
        if (newtype->type_map == NULL) {
            ulm_err(("Error: MPI_Type_struct: Out of memory\n"));
            rc = MPI_ERR_TYPE;
            _mpi_errhandler(MPI_COMM_WORLD, rc, __FILE__, __LINE__);
            return rc;
        }

        /*
         * Fill in new datatype's type_map....
         */
        typemap_i = 0;
        for (i = 0; i < count; i++) {
            if (types[i]->extent > 0) {
                typemap_off = array_of_displacements[i];
                for (j = 0; j < array_of_blocklengths[i]; j++) {
                    for (k = 0; k < types[i]->num_pairs; k++) {
                        new_size = types[i]->type_map[k].size;
                        new_offset = types[i]->type_map[k].offset +
                            typemap_off;
                        if ((typemap_i != 0)
                            &&
                            ((newtype->type_map[typemap_i - 1].size +
                              newtype->type_map[typemap_i - 1].offset) ==
                             new_offset)) {
                            /* consolidate entries - trim at ub */
                            newtype->type_map[typemap_i - 1].size +=
                                new_size;
                            if (_MPI_DTYPE_TRIM
                                && (newtype->type_map[typemap_i - 1].size +
                                    newtype->type_map[typemap_i -
                                                      1].offset > ub)) {
                                newtype->type_map[typemap_i - 1].size =
                                    ub - newtype->type_map[typemap_i -
                                                           1].offset;
                            }
                        } else {
                            if (!_MPI_DTYPE_TRIM
                                || ((new_offset + new_size > lb)
                                    && (new_offset < ub))) {
                                /* create new type_map entry if there is still something 
                                 * left after possible trimming */
                                if (_MPI_DTYPE_TRIM && new_offset < lb) {
                                    new_size -= (lb - new_offset);
                                    new_offset = lb;
                                }
                                if (_MPI_DTYPE_TRIM
                                    && new_offset + new_size > ub) {
                                    new_size = (ub - new_offset);
                                }
                                if (new_size > 0) {
                                    newtype->type_map[typemap_i].size =
                                        new_size;
                                    newtype->type_map[typemap_i].offset =
                                        new_offset;
                                    typemap_i++;
                                }
                            }
                        }
                    }
                    typemap_off += types[i]->extent;
                }
            }
        }
    } /* end if newtype->numpairs > 0 */
    else {
        newtype->type_map = NULL;
    }

    /* mark the datatype as contiguous if it clearly is ... */
    if (_MPI_MARK_AS_CONTIGUOUS) {
        if (((newtype->num_pairs == 0) && (newtype->extent == 0))
            || ((newtype->num_pairs == 1)
                && (newtype->extent == newtype->type_map[0].size))) {
            newtype->layout = CONTIGUOUS;
        }
    }

    *newdatatype = newtype;

    if (_mpi.fortran_layer_enabled) {
        newtype->fhandle = _mpi_ptr_table_add(_mpif.type_table, newtype);
    }

#endif

    return LAM_SUCCESS;
}

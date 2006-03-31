/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _PERUSE_H_
#define _PERUSE_H_

#include "mpi.h"

/* PERUSE type declarations */
typedef void* peruse_event_h;    /* Opaque event handle XXX */

typedef struct _peruse_comm_spec_t {
    MPI_Comm      comm;
    void *        buf;
    int           count;
    MPI_Datatype  datatype;
    int           peer;
    int           tag;
    int           operation;
} peruse_comm_spec_t;

typedef struct _peruse_file_spec_t {
    MPI_File      file;
    void *        buf;
    int           count;
    MPI_Datatype  datatype;
    MPI_Offset    offset;
    int           operation;
} peruse_file_spec_t;

typedef struct _peruse_win_spec_t {
    MPI_Win       win;
    void *        o_buf;
    int           o_count;
    MPI_Datatype  o_datatype;
    void *        t_buf;
    int           t_count;
    MPI_Datatype  t_datatype;
    MPI_Op        acc_op;
    int           peer;
    int           operation;
} peruse_win_spec_t;

typedef int (peruse_comm_callback_f)(peruse_event_h event_h,
              MPI_Aint unique_id, peruse_comm_spec_t * spec, void * param);

typedef int (peruse_file_callback_f)(peruse_event_h event_h,
              MPI_Aint unique_id, peruse_file_spec_t * spec, void * param);

typedef int (peruse_win_callback_f)(peruse_event_h event_h,
              MPI_Aint unique_id, peruse_win_spec_t * spec, void * param);


/* PERUSE constants */
enum {
    PERUSE_SUCCESS = 0,       /* Success *//* XXX Devation from 1.11 */
    PERUSE_ERR_INIT,          /* PERUSE initialization failure */
    PERUSE_ERR_GENERIC,       /* Generic unspecified error */
    PERUSE_ERR_MALLOC,        /* Memory-related error */
    PERUSE_ERR_EVENT,         /* Invalid event descriptor */
    PERUSE_ERR_EVENT_HANDLE,  /* Invalid event handle */
    PERUSE_ERR_PARAMETER,     /* Invalid input parameter */
    PERUSE_ERR_MPI_INIT,      /* MPI has not been initialized */
    PERUSE_ERR_COMM,          /* MPI_ERR_COMM class */
    PERUSE_ERR_FILE,          /* MPI_ERR_FILE class */
    PERUSE_ERR_WIN,           /* MPI_ERR_WIN class */
    PERUSE_ERR_MPI_OBJECT     /* Error with associated MPI object */
};

enum {
    PERUSE_EVENT_INVALID = -1, /* Must differ in value from PERUSE_SUCCESS. Devation from 1.11 */

    /* Point-to-point request events */
    PERUSE_COMM_REQ_ACTIVATE,
    PERUSE_COMM_REQ_MATCH_UNEX,
    PERUSE_COMM_REQ_INSERT_IN_POSTED_Q,
    PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
    PERUSE_COMM_REQ_XFER_BEGIN,
    PERUSE_COMM_REQ_XFER_CONTINUE,
    PERUSE_COMM_REQ_XFER_END,
    PERUSE_COMM_REQ_COMPLETE,
    PERUSE_COMM_REQ_NOTIFY,
    PERUSE_COMM_MSG_ARRIVED,
    PERUSE_COMM_MSG_INSERT_IN_UNEX_Q,
    PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q,
    PERUSE_COMM_MSG_MATCH_POSTED_REQ,

    /* Queue events*/
    PERUSE_COMM_SEARCH_POSTED_Q_BEGIN,
    PERUSE_COMM_SEARCH_POSTED_Q_END,
    PERUSE_COMM_SEARCH_UNEX_Q_BEGIN,    /* XXX Devation from 1.11 */
    PERUSE_COMM_SEARCH_UNEX_Q_END,

    /* Collective events */
    /* IO events */
    /* One-sided events */
    PERUSE_CUSTOM_EVENT
};

/* Scope of message queues */
enum {
    PERUSE_PER_COMM=0,                  /* XXX Devation from 1.11 */
    PERUSE_GLOBAL
};

/* Operation values */
enum {
    PERUSE_SEND=0,                      /* XXX Devation from 1.11 */
    PERUSE_RECV,
    PERUSE_PUT,
    PERUSE_GET,
    PERUSE_ACC,
    PERUSE_IO_READ,
    PERUSE_IO_WRITE
};

#define PERUSE_EVENT_HANDLE_NULL ((peruse_event_h)0)

/*
 * I. Environment
 */

/* PERUSE initialization */
int PERUSE_Init (void);

/* Query all implemented events */
int PERUSE_Query_supported_events (
        int * num_supported,
        char *** event_names,
        int ** events);

/* Query supported events */
int PERUSE_Query_event (const char * event_name, int *event);

/* Query event name */
int PERUSE_Query_event_name (int event, char ** event_name);

/* Get environment variables that affect MPI library behavior */
int PERUSE_Query_environment (int * env_size, char *** env);

/* Query the scope of queue metrics - global or per communicator */
int PERUSE_Query_queue_event_scope (int * scope);

/*
 * II. Events, objects initialization and manipulation
 */
/* Initialize event associated with an MPI communicator */
int PERUSE_Event_comm_register (
        int                       event,
        MPI_Comm                  comm,
        peruse_comm_callback_f *  callback_fn,
        void *                    param,
        peruse_event_h *          event_h);

/* Initialize event associated with an MPI file */
int PERUSE_Event_file_register (
        int                       event,
        MPI_File                  file,
        peruse_file_callback_f *  callback_fn,
        void *                    param,
        peruse_event_h *          event_h);

/* Initialize event associated with an MPI window */
int PERUSE_Event_win_register (
        int                       event,
        MPI_Win                   win,
        peruse_win_callback_f *   callback_fn,
        void *                    param,
        peruse_event_h *          event_h);

/* Start collecting data (activate event) */
int PERUSE_Event_activate (peruse_event_h event_h);

/* Stop collecting data (deactivate event) */
int PERUSE_Event_deactivate (peruse_event_h event_h);

/* Free event handle */
int PERUSE_Event_release (peruse_event_h * event_h);

/* Set a new comm callback */
int PERUSE_Event_comm_callback_set (
        peruse_event_h            event_h,
        peruse_comm_callback_f *  callback_fn,
        void *                    param);

/* Set a new file callback */
int PERUSE_Event_file_callback_set (
        peruse_event_h            event_h,
        peruse_file_callback_f *  callback_fn,
        void *                    param);

/* Set a new win callback */
 int PERUSE_Event_win_callback_set (
        peruse_event_h            event_h,
        peruse_win_callback_f *   callback_fn,
        void *                    param);

/* Get the current comm callback */
int PERUSE_Event_comm_callback_get (
        peruse_event_h            event_h,
        peruse_comm_callback_f ** callback_fn,
        void **                   param);

/* Get the current ile callback */
int PERUSE_Event_file_callback_get (
        peruse_event_h            event_h,
        peruse_file_callback_f ** callback_fn,
        void **                   param);

/* Get the current win callback */
int PERUSE_Event_win_callback_get (
        peruse_event_h            event_h,
        peruse_win_callback_f **  callback_fn,
        void **                   param);

/* Obtain event descriptor from an event handle (reverse lookup) */
int PERUSE_Event_get (peruse_event_h event_h, int * event);

/* Obtain MPI object associated with event handle */
/* XXX Shouldn't we have 3 different functions to obtain a specific object Comm/File/Win */
int PERUSE_Event_object_get (peruse_event_h event_h, void ** mpi_object);

/* Propagaiont mode */
int PERUSE_Event_propagate (peruse_event_h event_h, int mode);

#endif

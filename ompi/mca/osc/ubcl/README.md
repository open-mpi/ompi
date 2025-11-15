# INTRODUCTION

## How to use :
Configure using the `--with-ubcl=<path_to_ubcl>` command with a correct ubcl install.
It needs to find ubcl to build the component.
You cannot use the OSC/UBCL without the PML/UBCL as the former relies on the latter
for UBCL endpoints

The OSC/UBCL also relies on the opal/mca/common/ubcl and the ompi/mca/common/ubcl.



# Architecture :

## Most used data structures

Current UCBL API (for osc calls) resembles that of the MPI specification.
The idea is to offload as much as possible to UBCL, all this component needs to do
is translate arguments and make valid calls to UBCL.


The component is shared to all windows and mainly contains fields to help print log,
for now. The generic inherited class also brings necessary data to open the component
and create windows/modules such as *osc_init*, *osc_select* or *osc_finish*.


```c
struct mca_osc_ubcl_module_s {
    ompi_osc_base_module_t super;
    struct ompi_communicator_t *comm;
    struct ompi_win_t *win;
    int64_t wid;
    union {int *all; int uniq;} disp_unit;
    ubcl_win_flags_t win_flags;

    uint32_t same_disp_unit:1;
    uint32_t no_locks:1;
    uint32_t padding_infos:30;

    ubcl_win_sync_type_t sync_type;
    ubcl_win_sync_type_t *procs_sync_type;
    int64_t passive_lock_refcount;
    opal_mutex_t sync_lock;

    unsigned int nb_rank_waited;
    struct ompi_group_t *active_sync_access_group;
    struct ompi_group_t *active_sync_exposure_group;

    void *free_after;
};
typedef struct mca_osc_ubcl_module_s mca_osc_ubcl_module_t;
```

The module is specific to one window and it holds in fact the fields necessary to
that window such as the parent classes for example.
The super field holds the available one-sided communications while win holds the necessary
data to compute what's needed for the window at a higher level than the osc/ubcl.
We fill the function pointers of super by coppying them from a template established
in `osc_ubcl_component.c` file.
It means in theory that we could deactivate or switch API calls to some other one-sided
function but in pratice every window have the same calls.
The communicator field is a duplicated communicator of the one that was used to
start the window with. It is necessary info about the group of procs of the window,
in part in order to synchronize procs at window create/free using regular MPI
collectives without introducing deadlocks on the original communicator.
The wid is a unique id to identify the window.
The `win_flags` are essential on window creation and track for ubcl which channel
the window can use (*bxi*, *shm* or *self*).

```c
enum ubcl_win_sync_type {
    UBCL_WIN_SYNC_NONE,
    UBCL_WIN_SYNC_LOCK,
    UBCL_WIN_SYNC_LOCK_NO_CHECK,
    UBCL_WIN_SYNC_LOCK_ALL,
    UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK,
    UBCL_WIN_SYNC_PSCW,
    UBCL_WIN_SYNC_FENCE,
    UBCL_WIN_SYNC_FENCE_EPOCH
};
typedef enum ubcl_win_sync_type_t
```

`sync_type` and `procs_sync_type` are enums used to track the type of synchronization
used on the whole window and for each *access epoch* respectively. It has a debugging
purpose, checks correct use of the mpi interface and is also mandatory to handle
the various behaviors of the synchronization functions such as the `fence` epoch
only starting at the first communication.
`sync_type` is a global window status whereas `procs_sync_type` proxies the information
at the scale of each peer rank when needed by the sync type.
`sync_lock` is a lock used to guarantee only one thread access to the thread critical
fields (`sync_type`, `procs_sync_type`, `passive_lock_refcount`, `nb_tests_passed`).

`nb_tests_passed`is used exclusively by *Test* and *Wait* to track which exposure
epoch (to which proc) was terminated.
`active_sync_access_group` and `active_sync_exposure_group` save the group used
to create the pscw epoch(s). It is needed to complete the *Complete* *Wait* and
*Test* operations as well as the one-sided communications.
`Free_after` is a pointer to memory attached to the window that needs to be freed
alongside the window.



## Window creation

```c
MPI_Win_create
     ↓
ompi_win_create
     ↓
ompi_osc_base_select
     ↓          ↑  |
osc_ubcl_query -┘  |
                   ↓
           component_select
                   ↓
              new_module
                   ↓
               win_create
                   ↓
            ubcl_win_create
```

Each time the user requests a window to be created they will follow this diagram
of function calls. 
`osc_ubcl_query` returns the priority depending on the requested window flavor.
`ompi_osc_base_select` will select the component with the highest priority as the
osc for the window.
`component_select` calls the function to create the window/module and enforces synchronization with a barrier  
`new_module` allocates a new module and then copies the module template on itself
`win_create` prepares the `win_flags` for UBCL, based on the PML endpoints types of the communicator.
`ubcl_win_create` creates the window inside the UBCL library


### Dynamic windows

Giving the flavor `MPI_WIN_FLAVOR_DYNAMIC` allows OMPI to create a dynamic window.
We then need to attach a buffer (or more) to it with *win_attach* and *win_detach*.
Since the window buffer is handled by UBCL *win_attach* and *win_detach* do very little.


## Synchronization

### Generalities

To enable synchronization we need every procs involved to be inside the same window
and for an *epoch* to be opened.
To that effect we store the type of synchronization inside the `osc_module` which
means the type of the epoch opened (so either *passive* or *active* and which
one precisely) because we can't run a one-sided communication without any synchronization.

### Passive sync

In *passive synchronization*, data is moved from the memory of one process to the
memory of another, and only the origin process is explicitly involved in the transfer.
Thus, two origin processes may communicate by accessing the same location in a
target window.
Despite the fact that no MPI call is required, the target process still needs to call
`ubcl_progress` to actively handle the request to establish the *lock*.

#### Lock/Unlock

```c
int ompi_osc_ubcl_lock(int lock_type, int target, int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_unlock(int target, struct ompi_win_t *win);

int ompi_osc_ubcl_lock_all(int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_unlock_all(struct ompi_win_t *win);
```

The *lock*/*unlock* functions use the sync type `UBCL_WIN_SYNC_LOCK`. Lock is allowed
only if the window has a sync type `UBCL_WIN_SYNC_LOCK` or `UBCL_WIN_SYNC_NONE`
and if the target process is not yet locked. It marks the window (if not already
done) as sync type `UBCL_WIN_SYNC_LOCK`, and changes the target process sync type
to `UBCL_WIN_SYNC_LOCK` in the local array of locked processes.
It also increase the `passive_lock_refcount` which tracks the number of *locks*
done to allow *unlock* to reset the window type when it should.
*Unlock* requires the window to be in `UBCL_WIN_SYNC_LOCK` and have the target
process locked in the local array.

The *lock_all*/*unlock_all* functions use the sync type `UBCL_WIN_SYNC_LOCK_ALL`
for the window only because we don't locally mark target processes as locked.
Otherwise it function the same as a simple *lock*.
As MPI requires that an initiator cannot lock the same target multiple times,
*lock_all* and *lock* are mutually exclusive despite similar names which leads to
a different sync type needed.
The main difference is that the UBCL call requires an array in argument that we
have to build.


In case we're provided with `MPI_MODE_NOCHECK` as assertion, we don't bother
actually locking the processes. However we still mark the target process as being
locked with `UBCL_WIN_SYNC_LOCK_NO_CHECK` and change the window sync type to
`UBCL_WIN_SYNC_LOCK` in case of the simple *lock*.
For *lock_all* we mark the window as having an epoch `UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK`.


#### Flush

```c
int ompi_osc_ubcl_flush(int target, struct ompi_win_t *win);
int ompi_osc_ubcl_flush_all(struct ompi_win_t *win);
int ompi_osc_ubcl_flush_local(int target, struct ompi_win_t *win);
int ompi_osc_ubcl_flush_local_all(struct ompi_win_t *win);
```

The *flush* functions don't create any epoch and therefore don't have a sync type
associated. However, the *flush* functions can only be called if the current process has
a valid passive target access epoch on the target process.
They make sure that all the previous one-sided communications on the window,
from the initiator to the target, are completed.
As for now *flush_local* is an alias to *flush* and *flush[_local]_all* loops
on *flush[_local]*.


#### Sync

When the data modifications are not fully handled by the NIC, some counterproductive
caches must be cleaned at the start of one-sided exposure epochs.
In active target synchronization model, a call is made on target side in fence and post functions.
As there is no MPI call on target side in passive target synchronization model,
this is handled internally by ubcl.

### Active sync

In *active synchronization*, data is moved from the memory of one process to the
memory of another, and both are explicitly involved in the synchronization. This
communication pattern is similar to message passing, except that all the data transfer
arguments are provided by one process, and the second process only participates in
the synchronization.

#### PSCW

```c
int ompi_osc_ubcl_post(struct ompi_group_t *group, int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_start(struct ompi_group_t *group, int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_complete(struct ompi_win_t *win);
int ompi_osc_ubcl_wait(struct ompi_win_t *win);
int ompi_osc_ubcl_test(struct ompi_win_t *win, int *flag);
```

The *PSCW* functions use the sync type `UBCL_WIN_SYNC_PSCW`. *Post* and *Start* requires
sync type `UBCL_WIN_SYNC_NONE` or `UBCL_WIN_SYNC_PSCW` on the window and not
have a *PSCW* synchronization group tracked already. *Complete* and *Wait*
requires sync type `UBCL_WIN_SYNC_PSCW` on the window and a non-NULL synchronization
group in the window. *Test* is the same as *Wait* but non-blocking.

In UBCL, the functions only take one target at a time whereas OMPI PSCW functions
take a whole group. So *Post* and *Start* loop on the group given in argument and
the *Complete*, *Wait* and *Test* loop on the groups specified - and stored
inside the window - when the epoch was established.
Below is the correspondance list between UBCL and OMPI :
- `MPI_Win_post`     => `ubcl_win_target_grants_lock`
- `MPI_Win_start`    => `ubcl_win_initiator_waits_lock`
- `MPI_Win_complete` => `ubcl_win_initiator_releases_lock`
- `MPI_Win_wait`     => `ubcl_win_target_waits_lock_release`
- `MPI_Win_test`     => `ubcl_win_target_tests_lock_release`

We don't make use of the assert argument here for any of the active target synchronization functions.


#### Fence

```c
int ompi_osc_ubcl_fence(int assert, struct ompi_win_t *win);
```

The *fence* function uses the sync type `UBCL_WIN_SYNC_FENCE` and `UBCL_WIN_SYNC_FENCE_EPOCH`.
This synchronization scheme needs both types for correctness checks. No other
synchronization calls may be started unless all epochs are completed before.
The first fence sets the window sync type as `UBCL_WIN_SYNC_FENCE` and the first
one-sided communication that starts will begin a fence epoch, setting the sync type
to `UBCL_WIN_SYNC_FENCE_EPOCH`.
That also means we have to have to allow other synchronization schemes to start an
epoch on `UBCL_WIN_SYNC_FENCE` as if it's `UBCL_WIN_SYNC_NONE`.
The function flushes the one-sided communications started in the current epoch,
acting as a barrier. Additionally whencalled inside a *fence* epoch, it  closes
said epoch. The sync type is back to `UBCL_WIN_SYNC_FENCE`.

Here we take into account the `MPI_MODE_NOPRECEDE` and `MPI_MODE_NOSUCCEED`
assertions only. The first one allows us to skip flushing the previously started
one-sided communications since there are none. We don't exploit the second assertion
much except in the case where both values are given, then the *fence* doesn't do much.


## One-Sided Communicaions

### Put

```c
int ompi_osc_ubcl_put(const void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win);

int ompi_osc_ubcl_rput(const void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req);
```

### Get

```c
int ompi_osc_ubcl_get(void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win);

int ompi_osc_ubcl_rget(void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req);
```

### Atomic operations

```c
int ompi_osc_ubcl_accumulate(const void *origin_addr, int origin_count,
                             struct ompi_datatype_t *origin_dt, int target, ptrdiff_t target_disp,
                             int target_count, struct ompi_datatype_t *target_dt,
                             struct ompi_op_t *op, struct ompi_win_t *win);

int ompi_osc_ubcl_raccumulate(const void *origin_addr, int origin_count,
                              struct ompi_datatype_t *origin_dt, int target, ptrdiff_t target_disp,
                              int target_count, struct ompi_datatype_t *target_dt,
                              struct ompi_op_t *op, struct ompi_win_t *win,
                              struct ompi_request_t **request);

int ompi_osc_ubcl_get_accumulate(const void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_datatype, void *result_addr,
                                 int result_count, struct ompi_datatype_t *result_datatype,
                                 int target_rank, ptrdiff_t target_disp, int target_count,
                                 struct ompi_datatype_t *target_datatype, struct ompi_op_t *op,
                                 struct ompi_win_t *win);

int ompi_osc_ubcl_rget_accumulate(const void *origin_addr, int origin_count,
                                  struct ompi_datatype_t *origin_datatype, void *result_addr,
                                  int result_count, struct ompi_datatype_t *result_datatype,
                                  int target_rank, ptrdiff_t target_disp, int target_count,
                                  struct ompi_datatype_t *target_datatype, struct ompi_op_t *op,
                                  struct ompi_win_t *win, struct ompi_request_t **request);

int ompi_osc_ubcl_fetch_and_op(const void *origin_addr, void *result_addr,
                               struct ompi_datatype_t *dt, int target, ptrdiff_t target_disp,
                               struct ompi_op_t *op, struct ompi_win_t *win);

int ompi_osc_ubcl_compare_and_swap(const void *origin_addr, const void *compare_addr,
                                   void *result_addr, struct ompi_datatype_t *dt, int target,
                                   ptrdiff_t target_disp, struct ompi_win_t *win);
```

The implementation makes use of the similarity between these functions so *accumulate*
calls *raccumulate* wih *ompi_req = NULL*, *raccumulate* calls *rget_accumulate*
with all result argument sets to NULL or 0.
*get_accumulate* calls *rget_acucmulate* with *ompi_req = NULL*.
*fetch_op* also only needs to call *get_accumulate* with the correct arguments.
*compare_and_swap* gets its own implementation.




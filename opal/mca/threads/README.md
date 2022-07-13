# MCA THREADING FRAMEWORK

This MCA framework makes it possible to integrate new threading frameworks with the Open MPI runtime.

## BACKGROUND

There has been a lot of interest in integrating alternative threading models, in particular lightweight threading models with MPI implementations. Open MPI's modular component architecture seems like an ideal architecture for this sort of integration (in fact it was, Open MPI used to support Solaris and Windows threads).

Recently there has been interest in integrating MPI with lightweight tasking layers, which led to work reviving and modernizing the old modular threading code but with an emphasis on integrating lightweight threading models.

## SELECTING A THREADING MODEL

The threading model is chosen via the configure option `--with-threads=<threading_model>`. This will choose a compile time threading model as well as compiling the relevant MCA.

## IMPLEMENTATION

The MCA for threading libraries is implemented in two places, once as a set of `.h` files in `mca/threads/<threading_model>/threads_<threading_model>_{threads,mutex,tsd}.h` which are defined inline to the main thread implementation and also as an MCA component that is loaded at runtime.

For performance reasons, in particular synchronization overhead, it is not possible to implement a threading model as a traditional MCA. This means --at least in the short term-- that threading models are chosen at compile time rather than runtime options, using mechanisms similar to Open MPI's libevent integration.

The .h files are meant to be run on the fast path containing inline synchronization functions (threads_<threading_model>_mutex.h, thread local storage (threads_<threading_model>_tsd.h) and the opal_thread structure (threads_<threading_model>_thread.h).

The rest of the threading implementation follows the normal MCA model:

* `threads_<threading_model>_component.c` describes the version of the module and specifies the module open behavior (the threading model initialization goes here).

* `threads_<threading_model>_condition.c` defines an instance of `opal_condition_t` which is used by `condition.h` to define Open MPI specific condition variables.

* `threads_<threading_model>_module.c` defines the interface to opal's thread handle. It provides ways of comparing threads, getting the value of a thread via its handle and the implementation of thread local storage.

* `threads_<threading_model>_mutex.c` provides a slow path interface to creating and destroying mutices dynamically via mca allocation. They can also be defined statically using the `.h` fast path interface.

* `threads_<threading_model>_wait_sync.c` provides condition variable like waiting capability that ensures MPI progress while it waits.

## TODO

Some components in the current Open MPI runtime assume preemption and does not yield by default. Lightweight threading libraries typically require tasks to be cooperative and to voluntarily yield after some time.

Open MPI itself needs to be altered to use a common yielding model instead of usleep(3).

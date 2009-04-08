/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_MPIREG_H
#define _VT_MPIREG_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#define VT__MPI_ABORT                              0
#define VT__MPI_ADDRESS                            1
#define VT__MPI_ALLGATHER                          2
#define VT__MPI_ALLGATHERV                         3
#define VT__MPI_ALLREDUCE                          4
#define VT__MPI_ALLTOALL                           5
#define VT__MPI_ALLTOALLV                          6
#define VT__MPI_ATTR_DELETE                        7
#define VT__MPI_ATTR_GET                           8
#define VT__MPI_ATTR_PUT                           9  
#define VT__MPI_BARRIER                           10
#define VT__MPI_BCAST                             11
#define VT__MPI_BSEND                             12
#define VT__MPI_BSEND_INIT                        13
#define VT__MPI_BUFFER_ATTACH                     14
#define VT__MPI_BUFFER_DETACH                     15 
#define VT__MPI_CANCEL                            16
#define VT__MPI_CART_COORDS                       17
#define VT__MPI_CART_CREATE                       18
#define VT__MPI_CART_GET                          19
#define VT__MPI_CART_MAP                          20
#define VT__MPI_CART_RANK                         21
#define VT__MPI_CART_SHIFT                        22 
#define VT__MPI_CART_SUB                          23
#define VT__MPI_CARTDIM_GET                       24 
#define VT__MPI_COMM_COMPARE                      25
#define VT__MPI_COMM_CREATE                       26
#define VT__MPI_COMM_DUP                          27
#define VT__MPI_COMM_FREE                         28
#define VT__MPI_COMM_GROUP                        29
#define VT__MPI_COMM_RANK                         30
#define VT__MPI_COMM_REMOTE_GROUP                 31
#define VT__MPI_COMM_REMOTE_SIZE                  32
#define VT__MPI_COMM_SIZE                         33
#define VT__MPI_COMM_SPLIT                        34
#define VT__MPI_COMM_TEST_INTER                   35
#define VT__MPI_DIMS_CREATE                       36 
#define VT__MPI_ERRHANDLER_CREATE                 37
#define VT__MPI_ERRHANDLER_FREE                   38
#define VT__MPI_ERRHANDLER_GET                    39 
#define VT__MPI_ERRHANDLER_SET                    40
#define VT__MPI_ERROR_CLASS                       41 
#define VT__MPI_ERROR_STRING                      42
#define VT__MPI_FILE_CLOSE                        43
#define VT__MPI_FILE_IREAD                        44
#define VT__MPI_FILE_IREAD_AT                     45
#define VT__MPI_FILE_IREAD_SHARED                 46
#define VT__MPI_FILE_IWRITE                       47
#define VT__MPI_FILE_IWRITE_AT                    48
#define VT__MPI_FILE_IWRITE_SHARED                49
#define VT__MPI_FILE_OPEN                         50
#define VT__MPI_FILE_READ                         51
#define VT__MPI_FILE_READ_ALL                     52
#define VT__MPI_FILE_READ_ALL_BEGIN               53
#define VT__MPI_FILE_READ_ALL_END                 54
#define VT__MPI_FILE_READ_AT                      55
#define VT__MPI_FILE_READ_AT_ALL                  56
#define VT__MPI_FILE_READ_AT_ALL_BEGIN            57
#define VT__MPI_FILE_READ_AT_ALL_END              58
#define VT__MPI_FILE_READ_ORDERED                 59
#define VT__MPI_FILE_READ_ORDERED_BEGIN           60
#define VT__MPI_FILE_READ_ORDERED_END             61
#define VT__MPI_FILE_READ_SHARED                  62
#define VT__MPI_FILE_SEEK                         63
#define VT__MPI_FILE_SEEK_SHARED                  64
#define VT__MPI_FILE_WRITE                        65
#define VT__MPI_FILE_WRITE_ALL                    66
#define VT__MPI_FILE_WRITE_ALL_BEGIN              67
#define VT__MPI_FILE_WRITE_ALL_END                68
#define VT__MPI_FILE_WRITE_AT                     69
#define VT__MPI_FILE_WRITE_AT_ALL                 70
#define VT__MPI_FILE_WRITE_AT_ALL_BEGIN           71
#define VT__MPI_FILE_WRITE_AT_ALL_END             72
#define VT__MPI_FILE_WRITE_ORDERED                73
#define VT__MPI_FILE_WRITE_ORDERED_BEGIN          74
#define VT__MPI_FILE_WRITE_ORDERED_END            75
#define VT__MPI_FILE_WRITE_SHARED                 76
#define VT__MPI_FINALIZE                          77
#define VT__MPI_GATHER                            78
#define VT__MPI_GATHERV                           79
#define VT__MPI_GET_COUNT                         80
#define VT__MPI_GET_ELEMENTS                      81
#define VT__MPI_GET_PROCESSOR_NAME                82
#define VT__MPI_GET_VERSION                       83
#define VT__MPI_GRAPH_CREATE                      84
#define VT__MPI_GRAPH_GET                         85
#define VT__MPI_GRAPH_MAP                         86
#define VT__MPI_GRAPH_NEIGHBORS                   87
#define VT__MPI_GRAPH_NEIGHBORS_COUNT             88 
#define VT__MPI_GRAPHDIMS_GET                     89
#define VT__MPI_GROUP_COMPARE                     90
#define VT__MPI_GROUP_DIFFERENCE                  91
#define VT__MPI_GROUP_EXCL                        92 
#define VT__MPI_GROUP_FREE                        93
#define VT__MPI_GROUP_INCL                        94
#define VT__MPI_GROUP_INTERSECTION                95
#define VT__MPI_GROUP_RANGE_EXCL                  96
#define VT__MPI_GROUP_RANGE_INCL                  97
#define VT__MPI_GROUP_RANK                        98 
#define VT__MPI_GROUP_SIZE                        99    
#define VT__MPI_GROUP_TRANSLATE_RANKS            100
#define VT__MPI_GROUP_UNION                      101
#define VT__MPI_IBSEND                           102
#define VT__MPI_INIT                             103
#define VT__MPI_INIT_THREAD                      104
#define VT__MPI_INITIALIZED                      105
#define VT__MPI_INTERCOMM_CREATE                 106
#define VT__MPI_INTERCOMM_MERGE                  107
#define VT__MPI_IPROBE                           108
#define VT__MPI_IRECV                            109
#define VT__MPI_IRSEND                           110
#define VT__MPI_ISEND                            111
#define VT__MPI_ISSEND                           112
#define VT__MPI_KEYVAL_CREATE                    113
#define VT__MPI_KEYVAL_FREE                      114
#define VT__MPI_OP_CREATE                        115
#define VT__MPI_OP_FREE                          116
#define VT__MPI_PACK                             117
#define VT__MPI_PACK_SIZE                        118
#define VT__MPI_PCONTROL                         119
#define VT__MPI_PROBE                            120
#define VT__MPI_RECV                             121
#define VT__MPI_RECV_INIT                        122
#define VT__MPI_REDUCE                           123
#define VT__MPI_REDUCE_SCATTER                   124
#define VT__MPI_REQUEST_FREE                     125
#define VT__MPI_RSEND                            126
#define VT__MPI_RSEND_INIT                       127
#define VT__MPI_SCAN                             128
#define VT__MPI_SCATTER                          129
#define VT__MPI_SCATTERV                         130
#define VT__MPI_SEND                             131
#define VT__MPI_SEND_INIT                        132
#define VT__MPI_SENDRECV                         133
#define VT__MPI_SENDRECV_REPLACE                 134
#define VT__MPI_SSEND                            135 
#define VT__MPI_SSEND_INIT                       136
#define VT__MPI_START                            137
#define VT__MPI_STARTALL                         138
#define VT__MPI_TEST                             139
#define VT__MPI_TEST_CANCELLED                   140
#define VT__MPI_TESTALL                          141
#define VT__MPI_TESTANY                          142
#define VT__MPI_TESTSOME                         143   
#define VT__MPI_TOPO_TEST                        144                
#define VT__MPI_TYPE_COMMIT                      145 
#define VT__MPI_TYPE_CONTIGUOUS                  146
#define VT__MPI_TYPE_EXTENT                      147
#define VT__MPI_TYPE_FREE                        148 
#define VT__MPI_TYPE_HINDEXED                    149
#define VT__MPI_TYPE_HVECTOR                     150
#define VT__MPI_TYPE_INDEXED                     151
#define VT__MPI_TYPE_LB                          152
#define VT__MPI_TYPE_SIZE                        153
#define VT__MPI_TYPE_STRUCT                      154
#define VT__MPI_TYPE_UB                          155
#define VT__MPI_TYPE_VECTOR                      156 
#define VT__MPI_UNPACK                           157
#define VT__MPI_WAIT                             158
#define VT__MPI_WAITALL                          159   
#define VT__MPI_WAITANY                          160
#define VT__MPI_WAITSOME                         161
#define VT__MPI_WTICK                            162
#define VT__MPI_WTIME                            163
#define VT__MPI_ACCUMULATE                       164
#define VT__MPI_GET                              165
#define VT__MPI_PUT                              166
#define VT__MPI_WIN_COMPLETE                     167
#define VT__MPI_WIN_CREATE                       168
#define VT__MPI_WIN_FENCE                        169
#define VT__MPI_WIN_FREE                         170
#define VT__MPI_WIN_GET_GROUP                    171
#define VT__MPI_WIN_LOCK                         172
#define VT__MPI_WIN_POST                         173
#define VT__MPI_WIN_START                        174
#define VT__MPI_WIN_TEST                         175
#define VT__MPI_WIN_UNLOCK                       176
#define VT__MPI_WIN_WAIT                         177
#define VT__MPI_ALLTOALLW                        178
#define VT__MPI_EXSCAN                           179
#define VT__MPI_REGID_NUM                        180

extern int     vt_mpi_regid[VT__MPI_REGID_NUM];

EXTERN void    vt_mpi_register( void );

#endif










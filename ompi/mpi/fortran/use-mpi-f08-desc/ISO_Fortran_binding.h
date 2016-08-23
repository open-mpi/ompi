/************** Example ISO_Fortran_binding.h ********************/
#include <stdint.h>
#include <stddef.h>

/* Struct CFI_dim_t for triples of bound, extent and stride information */

typedef struct {
         intptr_t  lower_bound, 
                   extent,  
                   sm;
} CFI_dim_t;

typedef struct {
        intptr_t  lower_bound,
                  upper_bound,
                  stride;
}  CFI_bounds_t;

 
/* Maximum rank supported by the companion Fortran processor */

/* Changed from 15 to F2003 value of 7 (CER) */
#define CFI_MAX_RANK  7

/* Struct CFI_cdesc_t for holding all the information about a 
   descriptor-based Fortran object */

typedef struct {
  void *        base_addr;          /* base address of object                      */
  size_t        elem_len;           /* length of one element, in bytes             */
  int           rank;               /* object rank, 0 .. CF_MAX_RANK               */
  int           type;               /* identifier for type of object               */ 
  int           attribute;          /* object attribute: 0..2, or -1               */
  int           state;              /* allocation/association state: 0 or 1        */
//Removed (CER)
//void *        fdesc;              /* pointer to corresponding Fortran descriptor */
  CFI_dim_t     dim[CFI_MAX_RANK];  /* dimension triples                           */
} CFI_cdesc_t;


/* function prototypes */

int CFI_update_cdesc ( CFI_cdesc_t * );

int CFI_update_fdesc ( CFI_cdesc_t * );

int CFI_allocate     ( CFI_cdesc_t *, const CFI_bounds_t bounds[] );

int CFI_deallocate   ( CFI_cdesc_t * );

int CFI_is_contiguous   ( const CFI_cdesc_t *, _Bool * );

int CFI_bounds_to_cdesc ( const CFI_bounds_t bounds[] , CFI_cdesc_t * );

int CFI_cdesc_to_bounds ( const CFI_cdesc_t * , CFI_bounds_t bounds[] );


/* Sympolic names for attributes of objects   */

#define CFI_attribute_assumed         0
#define CFI_attribute_allocatable     1
#define CFI_attribute_pointer         2

/* Symbolic names for type identifiers */

#define CFI_type_unknown               0
#define CFI_type_struct              100
#define CFI_type_signed_char           1
#define CFI_type_short                 3
#define CFI_type_int                   5
#define CFI_type_long                  7
#define CFI_type_long_long             9
#define CFI_type_size_t               11
#define CFI_type_int8_t               12
#define CFI_type_int16_t              14
#define CFI_type_int32_t              16
#define CFI_type_int64_t              18
#define CFI_type_int_least8_t         20
#define CFI_type_int_least16_t        22
#define CFI_type_int_least32_t        24
#define CFI_type_int_least64_t        26
#define CFI_type_int_fast8_t          28
#define CFI_type_int_fast16_t         30
#define CFI_type_int_fast32_t         32
#define CFI_type_int_fast64_t         34
#define CFI_type_intmax_t             36
#define CFI_type_intptr_t             37
#define CFI_type_float                38
#define CFI_type_double               39
#define CFI_type_long_double          40
#define CFI_type_float_Complex        41
#define CFI_type_double_Complex       42
#define CFI_type_long_double_Complex  43
#define CFI_type_Bool                 44
#define CFI_type_char                 45

/* End of Example ISO_Fortran_binding.h */


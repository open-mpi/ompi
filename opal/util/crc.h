#ifndef _OPAL_CRC_H_
#define _OPAL_CRC_H_

#define CRC_POLYNOMIAL ((unsigned int)0x04c11db7)
#define CRC_INITIAL_REGISTER ((unsigned int)0xffffffff)

unsigned long
opal_bcopy_csum_partial(
    const void *  source,
    void *  destination,
    unsigned long copylen,
    unsigned long csumlen,
    unsigned long *  lastPartialLong,
    unsigned long *  lastPartialLength
    );

static inline unsigned long
opal_bcopy_csum (
    const void *  source,
    void *  destination,
    unsigned long copylen,
    unsigned long csumlen
    )
{
    unsigned long plong = 0;
    unsigned long plength = 0;
    return opal_bcopy_csum_partial(source, destination, copylen, csumlen, &plong, &plength);
}
                                                                                                                  
unsigned int 
opal_bcopy_uicsum_partial (
    const void *  source,
    void *  destination,
    unsigned long copylen,
    unsigned long csumlen,
    unsigned int *  lastPartialInt,
    unsigned int *  lastPartialLength
    );

static inline unsigned int
opal_bcopy_uicsum (
    const void *  source,
    void *  destination,
    unsigned long copylen,
    unsigned long csumlen
    )
{
    unsigned int pint = 0;
    unsigned int plength = 0;
    return opal_bcopy_uicsum_partial(source, destination, copylen, csumlen, &pint, &plength);
}
                                                                                                                  
unsigned long 
opal_csum_partial (
    const void *  source,
    unsigned long csumlen,
    unsigned long *  lastPartialLong,
    unsigned long *  lastPartialLength
    );


static inline unsigned long 
opal_csum(const void *  source, unsigned long csumlen)
{
    unsigned long lastPartialLong = 0;
    unsigned long lastPartialLength = 0;
    return opal_csum_partial(source, csumlen, &lastPartialLong, &lastPartialLength);
}

unsigned int
opal_uicsum_partial (
    const void *  source,
    unsigned long csumlen,
    unsigned int *  lastPartialInt,
    unsigned int *  lastPartialLength
    );

static inline unsigned int 
opal_uicsum(const void *  source, unsigned long csumlen)
{
    unsigned int lastPartialInt = 0;
    unsigned int lastPartialLength = 0;
    return opal_uicsum_partial(source, csumlen, &lastPartialInt, &lastPartialLength);
}
                                                                                                                  
/*
 * CRC Support
 */

void opal_initialize_crc_table(void);

static inline unsigned int 
opal_bcopy_uicrc_partial(
    const void *  source,
    void *  destination,
    unsigned long copylen,
    unsigned long crclen,
    unsigned int partial_crc);

static inline unsigned int 
opal_bcopy_uicrc(
    const void *  source, 
    void *  destination,
    unsigned long copylen, 
    unsigned long crclen)
{
    return opal_bcopy_uicrc_partial(source, destination, copylen, crclen, CRC_INITIAL_REGISTER);
}

unsigned int 
opal_uicrc_partial(
    const void *  source, 
    unsigned long crclen, 
    unsigned int partial_crc);


static inline unsigned int 
opal_uicrc(const void *  source, unsigned long crclen)
{
    return opal_uicrc_partial(source, crclen, CRC_INITIAL_REGISTER);
}
                                                                                                                  
#endif


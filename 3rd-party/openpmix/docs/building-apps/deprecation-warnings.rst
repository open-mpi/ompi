Deprecated Definitions
======================

The PMIx Standard will occasionally decide to deprecate and subsequently
remove attribute and constant definitions - and on rarer occasions, functional
APIs. The OpenPMIx library, however, maintains a policy of never actually
removing deprecated definitions. This is done with the intent of maintaining,
to the maximum extent possible, full backward compatibility.

Deprecated definitions are moved from their original header files to the
`pmix_deprecated.h` file to identify them as deprecated. While continuing
to be valid, users are encouraged to migrate away from these definitions
to remain in sync with the continued evolution of the PMIx Standard.

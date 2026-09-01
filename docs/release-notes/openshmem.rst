OpenSHMEM Functionality and Features
====================================

All OpenSHMEM-1.3 functionality is supported.

Static storage is symmetric only when it is backed by writable loadable
segments of the main executable.  Unrelated mappings, including private
anonymous mappings, are not static symmetric memory.

# Memory Layout
Memory is arranged into *chunks*. A chunk is:

* An i32 containing the address of the next chunk.
* An i32 containing the size of this chunk in bytes (minus the metadata, so 8 bytes fewer than is actually allocated)
* The contents of the subarray of size $size

Chunks are arranged in a sorted singly linked list.

The first word in memory is a pointer to the first allocated chunk.
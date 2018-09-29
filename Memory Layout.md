# Memory Layout
Memory is arranged into *chunks*. A chunk is:

* An i32 containing the address of the next chunk.
* An i32 containing the size of this chunk (minus the metadata)
* The contents of the subarray of size $size

Chunks are arranged in a sorted singly linked list.

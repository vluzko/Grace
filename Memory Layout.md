# Memory Layout
Memory is arranged into *chunks*. A chunk is:

* An i32 containing the address of the next chunk.
* An i32 containing the size of this chunk in bytes (minus the metadata, so 8 bytes fewer than is actually allocated)
* The contents of the subarray of size $size

Chunks are arranged in a sorted singly linked list.

The first word in memory is a pointer to the first allocated chunk.

The last chunk's "next chunk" pointer is 0.

If you're wondering why everything is in i32 it's because WebAssembly doesn't support operations on smaller values, and we haven't implement any compression yet.

## Example 1
There are two chunks, the first containing the array `[1, 2, 3]` and the second containing `[3, 4, 5]`. Memory (in words) is then:

    4 | 24 | 12 | 1 | 2 | 3 | 0 | 12 | 3 | 4 | 5

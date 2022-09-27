# Memory Management
## Memory Layout
Memory is arranged into *chunks*. A chunk is:

* An i32 containing the address of the next chunk.
* An i32 containing the size of this chunk in bytes (minus the metadata, so 8 bytes fewer than is actually allocated)
* A space we plan to use for an i32 with the number of references to the chunk.
* The contents of the subarray of size $size / the contents of the struct.

Chunks are arranged in a sorted singly linked list.

The first word in memory is a pointer to the first allocated chunk.

The last chunk's "next chunk" pointer is 0.

If you're wondering why everything is in i32 it's because WebAssembly doesn't support operations on smaller values, and we haven't implement any compression yet.

## Example 1
There are two chunks, the first containing the array `[1, 2, 3]` and the second containing `[3, 4, 5]`. Memory (in words) is then:

    4 | 24 | 12 | 1 | 2 | 3 | 0 | 12 | 3 | 4 | 5

## Garbage Collection
We haven't decided which garbage collection algorithm to use yet.


## Notes
* Write a bunch of functions in wast that do memory-management-y things (like resizing arrays and moving them around in
memory and so forth) at runtime; have the compiler add in a call to them whenever something requiring memory managment
is going to happen
* Garbage collection: reference counting. Have a reference-counter object that knows all the names of things that have
had memory allocated to them, and where that memory is and how much, and how many references there are to that name.
Also, a wrapper function within which all the other functions (e.g. ones users write, the memory-handling ones) are
called, and which calls the garbage collector after every function (or whenever).

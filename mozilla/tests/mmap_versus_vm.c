#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>

#ifndef MAP_ANONYMOUS
# define MAP_ANONYMOUS MAP_ANON
#endif

# include <libkern/OSAtomic.h>
# include <mach/mach_error.h>
# include <mach/mach_init.h>
# include <mach/vm_map.h>
# include <malloc/malloc.h>

const size_t CHUNK_SIZE = 256 * 1024;

static void
FillChunk(unsigned char *p)
{
#if 0
    size_t i;
    for (i = 0; i != CHUNK_SIZE; i += 4096) {
        if (*p)
            abort();
        p[i] = (unsigned char) i;
    }
#endif
}

int main(int argc, char *argv[])
{
    const size_t TOTAL_SIZE = 256 * 1024 * 1024;
    const size_t repeatCount = 50;
    size_t k, i;
    
    unsigned char *chunks[TOTAL_SIZE / CHUNK_SIZE];

    if (argc != 2)
        abort();
    bool use_vm = !strcmp(argv[1], "vm");
    if (!use_vm && strcmp(argv[1], "mmap"))
        abort();
    
    for (k = 0; k != repeatCount; ++k) {
        for (i = 0; i != TOTAL_SIZE/CHUNK_SIZE; ++i) {
            void *p;
            if (use_vm) {
                vm_address_t addr;
                kern_return_t err = vm_allocate((vm_map_t) mach_task_self(),
                                                &addr, (vm_size_t) CHUNK_SIZE, VM_FLAGS_ANYWHERE);
                if (err != KERN_SUCCESS)
                    abort();
                p = (void *) addr;
            } else {
                p = mmap(NULL, CHUNK_SIZE, PROT_READ|PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
                if (p == MAP_FAILED)
                    abort();
            }
            chunks[i] = p;
            FillChunk((unsigned char *) p);
        }
        for (i = 0; i != TOTAL_SIZE/CHUNK_SIZE; ++i) {
            void *p = chunks[i];
            if (use_vm) {
                vm_deallocate((vm_map_t) mach_task_self(),
                              (vm_address_t) p,
                              (vm_size_t) CHUNK_SIZE);
            } else {
                munmap(p, CHUNK_SIZE);
            }
        }
    }

    return 0;
}

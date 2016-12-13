#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>

#ifndef MAP_ANONYMOUS
# define MAP_ANONYMOUS MAP_ANON
#endif

const size_t CHUNK_SIZE = 1024 * 1024;

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
    const size_t BLOCK_SIZE = 64 * 1024 * 1024;
    const size_t TOTAL_SIZE = 256 * 1024 * 1024;
    const size_t CHUNKS_PER_BLOCK = BLOCK_SIZE / CHUNK_SIZE;
    const size_t repeatCount = 50;
    size_t k, i;
    
    if (argc != 2)
        abort();
    bool use_block = !strcmp(argv[1], "block");
    if (!use_block) {
        unsigned char *chunks[TOTAL_SIZE / CHUNK_SIZE];
        if (strcmp(argv[1], "noblock"))
            abort();

        for (k = 0; k != repeatCount; ++k) {
            for (i = 0; i != TOTAL_SIZE/CHUNK_SIZE; ++i) {
                void *p = mmap(NULL, CHUNK_SIZE, PROT_READ|PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
                chunks[i] = p;
                FillChunk(p);
            }
            for (i = 0; i != TOTAL_SIZE/CHUNK_SIZE; ++i) {
                munmap(chunks[i], CHUNK_SIZE);
            }
        }
        
    } else {
        unsigned char *blocks[TOTAL_SIZE / BLOCK_SIZE];
        for (i = 0; i != TOTAL_SIZE/BLOCK_SIZE; ++i) {
            blocks[i] = mmap(NULL, BLOCK_SIZE, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        }

        for (k = 0; k != repeatCount; ++k) {
            for (i = 0; i != TOTAL_SIZE/CHUNK_SIZE; ++i) {
                unsigned char *chunkAddress = blocks[i / CHUNKS_PER_BLOCK] +
                                              (i % CHUNKS_PER_BLOCK) * CHUNK_SIZE;

                void *p = mmap(chunkAddress, CHUNK_SIZE, PROT_READ|PROT_WRITE, MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
                FillChunk(p);
            }
            for (i = 0; i != TOTAL_SIZE/CHUNK_SIZE; ++i) {
                unsigned char *chunkAddress = blocks[i / CHUNKS_PER_BLOCK] +
                                              (i % CHUNKS_PER_BLOCK) * CHUNK_SIZE;
                mmap(chunkAddress, CHUNK_SIZE, PROT_NONE, MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            }
        }
#if 0
        for (i = 0; i != TOTAL_SIZE/BLOCK_SIZE; ++i) {
            munmap(blocks[i], BLOCK_SIZE);
        }
#endif
    }

    return 0;
}

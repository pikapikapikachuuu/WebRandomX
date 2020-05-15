#include "../randomx.h"
#include "../web_randomx.h"
#include <malloc.h>
#include <stdio.h>

EM_PORT_API(char*)  example() {
	const char myKey[] = "RandomX example key";
	const char myInput[] = "RandomX example input";

	// exchange data with js via memory
	char* hash = (char*)malloc(RANDOMX_HASH_SIZE * sizeof(char));
	if (NULL == hash) {
		return NULL;
	}

	randomx_flags flags = randomx_get_flags();
	randomx_cache *myCache = randomx_alloc_cache(flags);
	randomx_init_cache(myCache, &myKey, sizeof myKey);
	randomx_vm *myMachine = randomx_create_vm(flags, myCache, NULL);

	randomx_calculate_hash(myMachine, &myInput, sizeof myInput, hash);

	randomx_destroy_vm(myMachine);
	randomx_release_cache(myCache);

	return hash;
}

// release allocated memory
EM_PORT_API(void) freeBuff(void* buf) {
	free(buf);
}

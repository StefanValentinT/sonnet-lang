#ifndef DYNARRAY_H
#define DYNARRAY_H

#include <stddef.h>

typedef struct
{
	size count;
	size capacity;
	size elemSize;
	void* items;
} DynArray;

DynArray initArray(size capac, size elemSize);
void freeArray(DynArray* arr);
void appendArray(DynArray* arr, void* value);
void insertArray(DynArray* arr, size index, void* value);
void* getArray(DynArray* arr, size index);

#endif
#if __INCLUDE_LEVEL__ == 0

#include "log.c"
#include <stdlib.h>
#include <string.h>

#define GROWTH_FACTOR 2

DynArray initArray(size capacity, size elemSize)
{
	return (DynArray
	){.count = 0, .capacity = capacity, .elemSize = elemSize, .items = (void*)malloc(capacity * elemSize)};
}

void appendArray(DynArray* arr, void* value)
{
	if ((arr->count) >= arr->capacity)
	{
		arr->capacity *= GROWTH_FACTOR;
		void* temp = realloc(arr->items, (size_t)arr->capacity * arr->elemSize);
		if (temp == NULL)
		{
			logFatal("Could not allocate enough memory for dynamic array");
		}
		arr->items = temp;
	}
	char* pos = (char*)arr->items + (arr->count * arr->elemSize);
	memcpy(pos, value, arr->elemSize);
	arr->count++;
}

void* getArray(DynArray* arr, size index) {
	return (char*)arr->items + (index * arr->elemSize);
}

void freeArray(DynArray* arr)
{
	arr->count = 0;
	arr->capacity = 0;
	arr->elemSize = 0;
	free(arr->items);
}

#endif

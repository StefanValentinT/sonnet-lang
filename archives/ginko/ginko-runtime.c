#include <stdio.h>

void print_int(int val) {
	printf("%d\n", val);
}

void print_array(long* arr, long len) {
	printf("[");
	for (long i = 0; i < len; i++) {
		printf("%ld", arr[i]);
		if (i < len - 1) {
			printf(", ");
		}
	}
	printf("]\n");
}
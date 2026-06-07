#include <stdio.h>
#include <stdint.h>

int32_t print_i32(int32_t value) {
	printf("%d\n", value);
	fflush(stdout);
	return 0;
}

int32_t print_i64(int64_t value) {
	printf("%lld\n", value);
	fflush(stdout);
	return 0;
}

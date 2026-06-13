#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int32_t print_i8(int8_t value) {
	printf("%" PRIi8 "\n", value);
	return 0;
}

int32_t print_i16(int16_t value) {
	printf("%" PRIi16 "\n", value);
	return 0;
}


int32_t print_i32(int32_t value) {
	printf("%" PRIi32 "\n", value);
	return 0;
}

int32_t print_i64(int64_t value) {
	printf("%" PRIi64 "\n", value);
	return 0;
}

int32_t printsl_i8(int8_t value) {
	printf("%" PRIi8, value);
	return 0;
}

int32_t printsl_i16(int16_t value) {
	printf("%" PRIi16, value);
	return 0;
}


int32_t printsl_i32(int32_t value) {
	printf("%" PRIi32, value);
	return 0;
}

int32_t printsl_i64(int64_t value) {
	printf("%" PRIi64, value);
	return 0;
}


#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int32_t printsl_i8(int8_t value)   { printf("%" PRIi8, value); return 0; }
int32_t printsl_i16(int16_t value) { printf("%" PRIi16, value); return 0; }
int32_t printsl_i32(int32_t value) { printf("%" PRIi32, value); return 0; }
int32_t printsl_i64(int64_t value) { printf("%" PRIi64, value); return 0; }

int32_t print_i8(int8_t value)     { printf("%" PRIi8 "\n", value); return 0; }
int32_t print_i16(int16_t value)   { printf("%" PRIi16 "\n", value); return 0; }
int32_t print_i32(int32_t value)   { printf("%" PRIi32 "\n", value); return 0; }
int32_t print_i64(int64_t value)   { printf("%" PRIi64 "\n", value); return 0; }

int32_t printsl_u8(uint8_t value)   { printf("%" PRIu8, value); return 0; }
int32_t printsl_u16(uint16_t value) { printf("%" PRIu16, value); return 0; }
int32_t printsl_u32(uint32_t value) { printf("%" PRIu32, value); return 0; }
int32_t printsl_u64(uint64_t value) { printf("%" PRIu64, value); return 0; }

int32_t print_u8(uint8_t value)     { printf("%" PRIu8 "\n", value); return 0; }
int32_t print_u16(uint16_t value)   { printf("%" PRIu16 "\n", value); return 0; }
int32_t print_u32(uint32_t value)   { printf("%" PRIu32 "\n", value); return 0; }
int32_t print_u64(uint64_t value)   { printf("%" PRIu64 "\n", value); return 0; }

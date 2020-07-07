#include "fenv.h"

int fegetround(void) {
	return __softfloat_float_rounding_mode;
}

int fesetround(int __round) {
	__softfloat_float_rounding_mode = __round;
	return 0;
}
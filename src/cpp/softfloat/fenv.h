#pragma once

extern int __softfloat_float_rounding_mode;

#define	FE_TONEAREST	0x0000
#define	FE_TOWARDZERO	0x0001
#define	FE_UPWARD	0x0002
#define	FE_DOWNWARD	0x0003

int fegetround(void);
int fesetround(int __round);

#pragma once

#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <stdexcept>

#include "blake2/endian.h"
#include "softfloat.hpp"

constexpr int32_t unsigned32ToSigned2sCompl(uint32_t x) {
  return (-1 == ~0)
             ? (int32_t)x
             : (x > INT32_MAX ? (-(int32_t)(UINT32_MAX - x) - 1) : (int32_t)x);
}

constexpr int64_t unsigned64ToSigned2sCompl(uint64_t x) {
  return (-1 == ~0)
             ? (int64_t)x
             : (x > INT64_MAX ? (-(int64_t)(UINT64_MAX - x) - 1) : (int64_t)x);
}

constexpr uint64_t signExtend2sCompl(uint32_t x) {
  return (-1 == ~0)
             ? (int64_t)(int32_t)(x)
             : (x > INT32_MAX ? (x | 0xffffffff00000000ULL) : (uint64_t)x);
}

constexpr int RoundToNearest = 0;
constexpr int RoundDown = 1;
constexpr int RoundUp = 2;
constexpr int RoundToZero = 3;

#define rx_sqrt sqrt

typedef union {
  uint64_t u64[2];
  uint32_t u32[4];
  uint16_t u16[8];
  uint8_t u8[16];
} rx_vec_i128;

typedef union {
  struct {
    double lo;
    double hi;
  };
  rx_vec_i128 i;
} rx_vec_f128;

#define rx_aligned_alloc(a, b) malloc(a)
#define rx_aligned_free(a) free(a)
#define rx_prefetch_nta(x)
#define rx_prefetch_t0(x)

FORCE_INLINE rx_vec_f128 rx_load_vec_f128(const double* pd) {
  rx_vec_f128 x;
  x.i.u64[0] = load64(pd + 0);
  x.i.u64[1] = load64(pd + 1);
  return x;
}

FORCE_INLINE void rx_store_vec_f128(double* mem_addr, rx_vec_f128 a) {
  store64(mem_addr + 0, a.i.u64[0]);
  store64(mem_addr + 1, a.i.u64[1]);
}

FORCE_INLINE rx_vec_f128 rx_swap_vec_f128(rx_vec_f128 a) {
  double temp = a.hi;
  a.hi = a.lo;
  a.lo = temp;
  return a;
}

FORCE_INLINE rx_vec_f128 rx_add_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  softdouble rlo = softdouble(a.lo) + softdouble(b.lo);
  softdouble rhi = softdouble(a.hi) + softdouble(b.hi);
  x.lo = double(rlo);
  x.hi = double(rhi);
  return x;
  // rx_vec_f128 x;
	// x.lo = a.lo + b.lo;
	// x.hi = a.hi + b.hi;
	// return x;
}

FORCE_INLINE rx_vec_f128 rx_sub_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  softdouble rlo = softdouble(a.lo) - softdouble(b.lo);
  softdouble rhi = softdouble(a.hi) - softdouble(b.hi);
  x.lo = double(rlo);
  x.hi = double(rhi);
  return x;
}

FORCE_INLINE rx_vec_f128 rx_mul_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  softdouble rlo = softdouble(a.lo) * softdouble(b.lo);
  softdouble rhi = softdouble(a.hi) * softdouble(b.hi);
  x.lo = double(rlo);
  x.hi = double(rhi);
  return x;
}

FORCE_INLINE rx_vec_f128 rx_div_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  softdouble rlo = softdouble(a.lo) / softdouble(b.lo);
  softdouble rhi = softdouble(a.hi) / softdouble(b.hi);
  x.lo = double(rlo);
  x.hi = double(rhi);
  return x;
}

FORCE_INLINE rx_vec_f128 rx_sqrt_vec_f128(rx_vec_f128 a) {
  rx_vec_f128 x;
  softdouble rlo = sqrt(softdouble(a.lo));
  softdouble rhi = sqrt(softdouble(a.hi));
  x.lo = double(rlo);
  x.hi = double(rhi);
  return x;
}

FORCE_INLINE rx_vec_i128 rx_set1_long_vec_i128(uint64_t a) {
  rx_vec_i128 x;
  x.u64[0] = a;
  x.u64[1] = a;
  return x;
}

FORCE_INLINE rx_vec_f128 rx_vec_i128_vec_f128(rx_vec_i128 a) {
  rx_vec_f128 x;
  x.i = a;
  return x;
}

FORCE_INLINE rx_vec_f128 rx_set_vec_f128(uint64_t x1, uint64_t x0) {
  rx_vec_f128 v;
  v.i.u64[0] = x0;
  v.i.u64[1] = x1;
  return v;
}

FORCE_INLINE rx_vec_f128 rx_set1_vec_f128(uint64_t x) {
  rx_vec_f128 v;
  v.i.u64[0] = x;
  v.i.u64[1] = x;
  return v;
}

FORCE_INLINE rx_vec_f128 rx_xor_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  x.i.u64[0] = a.i.u64[0] ^ b.i.u64[0];
  x.i.u64[1] = a.i.u64[1] ^ b.i.u64[1];
  return x;
}

FORCE_INLINE rx_vec_f128 rx_and_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  x.i.u64[0] = a.i.u64[0] & b.i.u64[0];
  x.i.u64[1] = a.i.u64[1] & b.i.u64[1];
  return x;
}

FORCE_INLINE rx_vec_f128 rx_or_vec_f128(rx_vec_f128 a, rx_vec_f128 b) {
  rx_vec_f128 x;
  x.i.u64[0] = a.i.u64[0] | b.i.u64[0];
  x.i.u64[1] = a.i.u64[1] | b.i.u64[1];
  return x;
}

FORCE_INLINE int rx_vec_i128_x(rx_vec_i128 a) { return a.u32[0]; }

FORCE_INLINE int rx_vec_i128_y(rx_vec_i128 a) { return a.u32[1]; }

FORCE_INLINE int rx_vec_i128_z(rx_vec_i128 a) { return a.u32[2]; }

FORCE_INLINE int rx_vec_i128_w(rx_vec_i128 a) { return a.u32[3]; }

FORCE_INLINE rx_vec_i128 rx_set_int_vec_i128(int _I3, int _I2, int _I1,
                                             int _I0) {
  rx_vec_i128 v;
  v.u32[0] = _I0;
  v.u32[1] = _I1;
  v.u32[2] = _I2;
  v.u32[3] = _I3;
  return v;
};

FORCE_INLINE rx_vec_i128 rx_xor_vec_i128(rx_vec_i128 _A, rx_vec_i128 _B) {
  rx_vec_i128 c;
  c.u32[0] = _A.u32[0] ^ _B.u32[0];
  c.u32[1] = _A.u32[1] ^ _B.u32[1];
  c.u32[2] = _A.u32[2] ^ _B.u32[2];
  c.u32[3] = _A.u32[3] ^ _B.u32[3];
  return c;
}

FORCE_INLINE rx_vec_i128 rx_load_vec_i128(rx_vec_i128 const* _P) {
#if defined(NATIVE_LITTLE_ENDIAN)
  return *_P;
#else
  uint32_t* ptr = (uint32_t*)_P;
  rx_vec_i128 c;
  c.u32[0] = load32(ptr + 0);
  c.u32[1] = load32(ptr + 1);
  c.u32[2] = load32(ptr + 2);
  c.u32[3] = load32(ptr + 3);
  return c;
#endif
}

FORCE_INLINE void rx_store_vec_i128(rx_vec_i128* _P, rx_vec_i128 _B) {
#if defined(NATIVE_LITTLE_ENDIAN)
  *_P = _B;
#else
  uint32_t* ptr = (uint32_t*)_P;
  store32(ptr + 0, _B.u32[0]);
  store32(ptr + 1, _B.u32[1]);
  store32(ptr + 2, _B.u32[2]);
  store32(ptr + 3, _B.u32[3]);
#endif
}

FORCE_INLINE rx_vec_f128 rx_cvt_packed_int_vec_f128(const void* addr) {
  rx_vec_f128 x;
  x.lo = (double)unsigned32ToSigned2sCompl(load32((uint8_t*)addr + 0));
  x.hi = (double)unsigned32ToSigned2sCompl(load32((uint8_t*)addr + 4));
  return x;
}

#define RANDOMX_DEFAULT_FENV

#ifndef HAVE_AES
static const char* platformError = "Platform doesn't support hardware AES";

#include <stdexcept>

FORCE_INLINE rx_vec_i128 rx_aesenc_vec_i128(rx_vec_i128 v, rx_vec_i128 rkey) {
  throw std::runtime_error(platformError);
}

FORCE_INLINE rx_vec_i128 rx_aesdec_vec_i128(rx_vec_i128 v, rx_vec_i128 rkey) {
  throw std::runtime_error(platformError);
}

#define HAVE_AES 0

#endif

#ifdef RANDOMX_DEFAULT_FENV
void rx_reset_float_state();
void rx_set_rounding_mode(uint32_t mode);
#endif

uint64_t mulh(uint64_t, uint64_t);
int64_t smulh(int64_t, int64_t);
uint64_t rotl(uint64_t, unsigned int);
uint64_t rotr(uint64_t, unsigned int);

#include "softfloat.hpp"

enum { tininess_beforeRounding = 0, tininess_afterRounding = 1 };
// TODO
static const uint_fast8_t globalDetectTininess = tininess_afterRounding;

enum {
  round_near_even = 0,  // round to nearest, with ties to even
  round_minMag = 1,     // round to minimum magnitude (toward zero)
  round_min = 2,        // round to minimum (down)
  round_max = 3,        // round to maximum (up)
  round_near_maxMag =
      4,  // round to nearest, with ties to maximum magnitude (away from zero)
  round_odd = 5  // round to odd (jamming)
};
// TODO
static const uint_fast8_t globalRoundingMode = round_near_even;

enum {
  flag_inexact = 1,
  flag_underflow = 2,
  flag_overflow = 4,
  flag_infinite = 8,
  flag_invalid = 16
};

// Disabled to make softfloat code stateless
// May be changed in the future for better error handling
static inline void raiseFlags(uint_fast8_t /* flags */) {
  // exceptionFlags |= flags;
}

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
#define signF64UI(a) (((uint64_t)(a) >> 63) != 0)
#define expF64UI(a) ((int_fast16_t)((a) >> 52) & 0x7FF)
#define fracF64UI(a) ((a)&UINT64_C(0x000FFFFFFFFFFFFF))
#define packToF64UI(sign, exp, sig)                                          \
  ((uint64_t)(((uint_fast64_t)(sign) << 63) + ((uint_fast64_t)(exp) << 52) + \
              (sig)))
#define isNaNF64UI(a)                            \
  (((~(a)&UINT64_C(0x7FF0000000000000)) == 0) && \
   ((a)&UINT64_C(0x000FFFFFFFFFFFFF)))

typedef softdouble float64_t;

// Integer-to-floating-point conversion routines
static float64_t ui32_to_f64(uint32_t);
static float64_t ui64_to_f64(uint64_t);
static float64_t i32_to_f64(int32_t);
static float64_t i64_to_f64(int64_t);

// Floating-point operations
static int_fast32_t f64_to_i32(float64_t, uint_fast8_t, bool);
static int_fast64_t f64_to_i64(float64_t, uint_fast8_t, bool);
static int_fast32_t f64_to_i32_r_minMag(float64_t, bool);
static float64_t f64_roundToInt(float64_t, uint_fast8_t, bool);
static float64_t f64_add(float64_t, float64_t);
static float64_t f64_sub(float64_t, float64_t);
static float64_t f64_mul(float64_t, float64_t);
static float64_t f64_div(float64_t, float64_t);
static float64_t f64_rem(float64_t, float64_t);
static float64_t f64_sqrt(float64_t);
static bool f64_eq(float64_t, float64_t);
static bool f64_le(float64_t, float64_t);
static bool f64_lt(float64_t, float64_t);

// Softdouble methods and members
softdouble::softdouble(const uint32_t a) { *this = ui32_to_f64(a); }
softdouble::softdouble(const uint64_t a) { *this = ui64_to_f64(a); }
softdouble::softdouble(const int32_t a) { *this = i32_to_f64(a); }
softdouble::softdouble(const int64_t a) { *this = i64_to_f64(a); }

softdouble softdouble::operator+(const softdouble& a) const {
  return f64_add(*this, a);
}
softdouble softdouble::operator-(const softdouble& a) const {
  return f64_sub(*this, a);
}
softdouble softdouble::operator*(const softdouble& a) const {
  return f64_mul(*this, a);
}
softdouble softdouble::operator/(const softdouble& a) const {
  return f64_div(*this, a);
}
softdouble softdouble::operator%(const softdouble& a) const {
  return f64_rem(*this, a);
}

bool softdouble::operator==(const softdouble& a) const {
  return f64_eq(*this, a);
}
bool softdouble::operator!=(const softdouble& a) const {
  return !f64_eq(*this, a);
}
bool softdouble::operator>(const softdouble& a) const {
  return f64_lt(a, *this);
}
bool softdouble::operator>=(const softdouble& a) const {
  return f64_le(a, *this);
}
bool softdouble::operator<(const softdouble& a) const {
  return f64_lt(*this, a);
}
bool softdouble::operator<=(const softdouble& a) const {
  return f64_le(*this, a);
}

// Overloads for math function(s)
softdouble sqrt(const softdouble& a) { return f64_sqrt(a); }

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
#define defaultNaNF64UI UINT64_C(0xFFF8000000000000)
#define softfloat_isSigNaNF64UI(uiA)                                         \
  ((((uiA)&UINT64_C(0x7FF8000000000000)) == UINT64_C(0x7FF0000000000000)) && \
   ((uiA)&UINT64_C(0x0007FFFFFFFFFFFF)))
static uint_fast64_t softfloat_propagateNaNF64UI(uint_fast64_t uiA,
                                                 uint_fast64_t uiB);

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
static float64_t softfloat_roundPackToF64(bool, int_fast16_t, uint_fast64_t);
static float64_t softfloat_normRoundPackToF64(bool, int_fast16_t,
                                              uint_fast64_t);

static float64_t softfloat_addMagsF64(uint_fast64_t, uint_fast64_t, bool);
static float64_t softfloat_subMagsF64(uint_fast64_t, uint_fast64_t, bool);

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
static inline uint64_t softfloat_shiftRightJam64(uint64_t a,
                                                 uint_fast32_t dist) {
  // fixed unsigned unary minus: -x == ~x + 1
  return (dist < 63) ? a >> dist | ((uint64_t)(a << ((~dist + 1) & 63)) != 0)
                     : (a != 0);
}

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
static const uint_least8_t softfloat_countLeadingZeros8[256] = {
    8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static uint_fast8_t softfloat_countLeadingZeros64(uint64_t a);

/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------
 *----------------------------------------------------------------------------*/

static float64_t f64_add(float64_t a, float64_t b) {
  uint_fast64_t uiA;
  bool signA;
  uint_fast64_t uiB;
  bool signB;

  uiA = a.v;
  signA = signF64UI(uiA);
  uiB = b.v;
  signB = signF64UI(uiB);
  if (signA == signB) {
    return softfloat_addMagsF64(uiA, uiB, signA);
  } else {
    return softfloat_subMagsF64(uiA, uiB, signA);
  }
}

static uint_fast64_t softfloat_propagateNaNF64UI(uint_fast64_t uiA,
                                                 uint_fast64_t uiB) {
  bool isSigNaNA;

  isSigNaNA = softfloat_isSigNaNF64UI(uiA);
  if (isSigNaNA || softfloat_isSigNaNF64UI(uiB)) {
    raiseFlags(flag_invalid);
    if (isSigNaNA) return uiA | UINT64_C(0x0008000000000000);
  }
  return (isNaNF64UI(uiA) ? uiA : uiB) | UINT64_C(0x0008000000000000);
}

static float64_t softfloat_roundPackToF64(bool sign, int_fast16_t exp,
                                          uint_fast64_t sig) {
  uint_fast8_t roundingMode;
  bool roundNearEven;
  uint_fast16_t roundIncrement, roundBits;
  bool isTiny;
  uint_fast64_t uiZ;

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  roundingMode = globalRoundingMode;
  roundNearEven = (roundingMode == round_near_even);
  roundIncrement = 0x200;
  if (!roundNearEven && (roundingMode != round_near_maxMag)) {
    roundIncrement =
        (roundingMode == (sign ? round_min : round_max)) ? 0x3FF : 0;
  }
  roundBits = sig & 0x3FF;
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  if (0x7FD <= (uint16_t)exp) {
    if (exp < 0) {
      /*----------------------------------------------------------------
       *----------------------------------------------------------------*/
      isTiny = (globalDetectTininess == tininess_beforeRounding) ||
               (exp < -1) ||
               (sig + roundIncrement < UINT64_C(0x8000000000000000));
      sig = softfloat_shiftRightJam64(sig, -exp);
      exp = 0;
      roundBits = sig & 0x3FF;
      if (isTiny && roundBits) {
        raiseFlags(flag_underflow);
      }
    } else if ((0x7FD < exp) ||
               (UINT64_C(0x8000000000000000) <= sig + roundIncrement)) {
      /*----------------------------------------------------------------
       *----------------------------------------------------------------*/
      raiseFlags(flag_overflow | flag_inexact);
      uiZ = packToF64UI(sign, 0x7FF, 0) - !roundIncrement;
      goto uiZ;
    }
  }
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  sig = (sig + roundIncrement) >> 10;
  if (roundBits) {
    raiseFlags(flag_inexact);
    if (roundingMode == round_odd) {
      sig |= 1;
      goto packReturn;
    }
  }
  sig &= ~(uint_fast64_t)(!(roundBits ^ 0x200) & roundNearEven);
  if (!sig) exp = 0;
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
packReturn:
  uiZ = packToF64UI(sign, exp, sig);
uiZ:
  return float64_t::fromRaw(uiZ);
}

static float64_t softfloat_normRoundPackToF64(bool sign, int_fast16_t exp,
                                              uint_fast64_t sig) {
  int_fast8_t shiftDist;

  shiftDist = softfloat_countLeadingZeros64(sig) - 1;
  exp -= shiftDist;
  if ((10 <= shiftDist) && ((unsigned int)exp < 0x7FD)) {
    return float64_t::fromRaw(
        packToF64UI(sign, sig ? exp : 0, sig << (shiftDist - 10)));
  } else {
    return softfloat_roundPackToF64(sign, exp, sig << shiftDist);
  }
}

static float64_t softfloat_addMagsF64(uint_fast64_t uiA, uint_fast64_t uiB,
                                      bool signZ) {
  int_fast16_t expA;
  uint_fast64_t sigA;
  int_fast16_t expB;
  uint_fast64_t sigB;
  int_fast16_t expDiff;
  uint_fast64_t uiZ;
  int_fast16_t expZ;
  uint_fast64_t sigZ;

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  expA = expF64UI(uiA);
  sigA = fracF64UI(uiA);
  expB = expF64UI(uiB);
  sigB = fracF64UI(uiB);
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  expDiff = expA - expB;
  if (!expDiff) {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    if (!expA) {
      uiZ = uiA + sigB;
      goto uiZ;
    }
    if (expA == 0x7FF) {
      if (sigA | sigB) goto propagateNaN;
      uiZ = uiA;
      goto uiZ;
    }
    expZ = expA;
    sigZ = UINT64_C(0x0020000000000000) + sigA + sigB;
    sigZ <<= 9;
  } else {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    sigA <<= 9;
    sigB <<= 9;
    if (expDiff < 0) {
      if (expB == 0x7FF) {
        if (sigB) goto propagateNaN;
        uiZ = packToF64UI(signZ, 0x7FF, 0);
        goto uiZ;
      }
      expZ = expB;
      if (expA) {
        sigA += UINT64_C(0x2000000000000000);
      } else {
        sigA <<= 1;
      }
      sigA = softfloat_shiftRightJam64(sigA, -expDiff);
    } else {
      if (expA == 0x7FF) {
        if (sigA) goto propagateNaN;
        uiZ = uiA;
        goto uiZ;
      }
      expZ = expA;
      if (expB) {
        sigB += UINT64_C(0x2000000000000000);
      } else {
        sigB <<= 1;
      }
      sigB = softfloat_shiftRightJam64(sigB, expDiff);
    }
    sigZ = UINT64_C(0x2000000000000000) + sigA + sigB;
    if (sigZ < UINT64_C(0x4000000000000000)) {
      --expZ;
      sigZ <<= 1;
    }
  }
  return softfloat_roundPackToF64(signZ, expZ, sigZ);
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
propagateNaN:
  uiZ = softfloat_propagateNaNF64UI(uiA, uiB);
uiZ:
  return float64_t::fromRaw(uiZ);
}

static float64_t softfloat_subMagsF64(uint_fast64_t uiA, uint_fast64_t uiB,
                                      bool signZ) {
  int_fast16_t expA;
  uint_fast64_t sigA;
  int_fast16_t expB;
  uint_fast64_t sigB;
  int_fast16_t expDiff;
  uint_fast64_t uiZ;
  int_fast64_t sigDiff;
  int_fast8_t shiftDist;
  int_fast16_t expZ;
  uint_fast64_t sigZ;

  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  expA = expF64UI(uiA);
  sigA = fracF64UI(uiA);
  expB = expF64UI(uiB);
  sigB = fracF64UI(uiB);
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
  expDiff = expA - expB;
  if (!expDiff) {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    if (expA == 0x7FF) {
      if (sigA | sigB) goto propagateNaN;
      raiseFlags(flag_invalid);
      uiZ = defaultNaNF64UI;
      goto uiZ;
    }
    sigDiff = sigA - sigB;
    if (!sigDiff) {
      uiZ = packToF64UI((globalRoundingMode == round_min), 0, 0);
      goto uiZ;
    }
    if (expA) --expA;
    if (sigDiff < 0) {
      signZ = !signZ;
      sigDiff = -sigDiff;
    }
    shiftDist = softfloat_countLeadingZeros64(sigDiff) - 11;
    expZ = expA - shiftDist;
    if (expZ < 0) {
      shiftDist = (int_fast8_t)expA;  // fixed type cast
      expZ = 0;
    }
    uiZ = packToF64UI(signZ, expZ, sigDiff << shiftDist);
    goto uiZ;
  } else {
    /*--------------------------------------------------------------------
     *--------------------------------------------------------------------*/
    sigA <<= 10;
    sigB <<= 10;
    if (expDiff < 0) {
      /*----------------------------------------------------------------
       *----------------------------------------------------------------*/
      signZ = !signZ;
      if (expB == 0x7FF) {
        if (sigB) goto propagateNaN;
        uiZ = packToF64UI(signZ, 0x7FF, 0);
        goto uiZ;
      }
      sigA += expA ? UINT64_C(0x4000000000000000) : sigA;
      sigA = softfloat_shiftRightJam64(sigA, -expDiff);
      sigB |= UINT64_C(0x4000000000000000);
      expZ = expB;
      sigZ = sigB - sigA;
    } else {
      /*----------------------------------------------------------------
       *----------------------------------------------------------------*/
      if (expA == 0x7FF) {
        if (sigA) goto propagateNaN;
        uiZ = uiA;
        goto uiZ;
      }
      sigB += expB ? UINT64_C(0x4000000000000000) : sigB;
      sigB = softfloat_shiftRightJam64(sigB, expDiff);
      sigA |= UINT64_C(0x4000000000000000);
      expZ = expA;
      sigZ = sigA - sigB;
    }
    return softfloat_normRoundPackToF64(signZ, expZ - 1, sigZ);
  }
  /*------------------------------------------------------------------------
   *------------------------------------------------------------------------*/
propagateNaN:
  uiZ = softfloat_propagateNaNF64UI(uiA, uiB);
uiZ:
  return float64_t::fromRaw(uiZ);
}

static uint_fast8_t softfloat_countLeadingZeros64(uint64_t a) {
  uint_fast8_t count;
  uint32_t a32;

  count = 0;
  a32 = a >> 32;
  if (!a32) {
    count = 32;
    a32 = (uint32_t)a;  // fixed warning on type cast
  }
  /*------------------------------------------------------------------------
  | From here, result is current count + count leading zeros of `a32'.
  *------------------------------------------------------------------------*/
  if (a32 < 0x10000) {
    count += 16;
    a32 <<= 16;
  }
  if (a32 < 0x1000000) {
    count += 8;
    a32 <<= 8;
  }
  count += softfloat_countLeadingZeros8[a32 >> 24];
  return count;
}

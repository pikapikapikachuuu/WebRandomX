#include <cstdint>

// For faster memory accessing
typedef union suf64 {
  int64_t i;
  uint64_t u;
  double f;
} suf64;

struct softdouble {
 public:
  // constructors
  softdouble() { v = 0; }
  softdouble(const softdouble& s) { v = s.v; }
  softdouble& operator=(const softdouble& s) {
    if (&s != this)
      v = s.v;
    return *this;
  }

  // Construct from raw binary representation
  static const softdouble fromRaw(const uint64_t a) {
    softdouble x;
    x.v = a;
    return x;
  }

  // Construct from integer
  explicit softdouble(const uint32_t);
  explicit softdouble(const uint64_t);
  explicit softdouble(const int32_t);
  explicit softdouble(const int64_t);

  // Construct from double
  explicit softdouble(const double a) {
    suf64 s;
    s.f = a;
    v = s.u;
  }

  // Type case
  operator double() const {
    suf64 s;
    s.u = v;
    return s.f;
  }

  // Basic arithmetics
  softdouble operator+(const softdouble&) const;
  softdouble operator-(const softdouble&) const;
  softdouble operator*(const softdouble&) const;
  softdouble operator/(const softdouble&) const;
  softdouble operator-() const {
    softdouble x;
    x.v = v ^ (1ULL << 63);
    return x;
  }

  /**
  A quote from original softdouble manual:

  > The IEEE Standard remainder operation computes the value
  > a - n * b, where n is the integer closest to a / b.
  > If a / b is exactly halfway between two integers, n is the even integer
  > closest to a / b. The IEEE Standard’s remainder operation is always exact
  and so requires no rounding.
  > Depending on the relative magnitudes of the
  operands, the remainder functions can take considerably longer to execute
  than the other softdouble functions. This is an inherent characteristic of
  the remainder operation itself and is not a flaw in the softdouble
  implementation.
  */
  softdouble operator%(const softdouble&) const;

  softdouble& operator+=(const softdouble& a) {
    *this = *this + a;
    return *this;
  }
  softdouble& operator-=(const softdouble& a) {
    *this = *this - a;
    return *this;
  }
  softdouble& operator*=(const softdouble& a) {
    *this = *this * a;
    return *this;
  }
  softdouble& operator/=(const softdouble& a) {
    *this = *this / a;
    return *this;
  }
  softdouble& operator%=(const softdouble& a) {
    *this = *this % a;
    return *this;
  }

  // Comparison operations
  // Any operation with NaN produces false. The only exception is when x is NaN:
  // x != y for any y. Positive and negative zeros are equal.
  bool operator==(const softdouble&) const;
  bool operator!=(const softdouble&) const;
  bool operator>(const softdouble&) const;
  bool operator>=(const softdouble&) const;
  bool operator<(const softdouble&) const;
  bool operator<=(const softdouble&) const;

  // Indicators
  inline bool isNaN() const {
    return (v & 0x7fffffffffffffff) > 0x7ff0000000000000;
  }
  inline bool isInf() const {
    return (v & 0x7fffffffffffffff) == 0x7ff0000000000000;
  }
  inline bool isSubnormal() const { return ((v >> 52) & 0x7FF) == 0; }

  // Sign bit
  inline bool getSign() const { return (v >> 63) != 0; }
  inline softdouble setSign(bool sign) const {
    softdouble x;
    x.v = (v & ((1ULL << 63) - 1)) | ((uint_fast64_t)(sign) << 63);
    return x;
  }

  // 0-based exponent
  inline int getExp() const { return ((v >> 52) & 0x7FF) - 1023; }
  inline softdouble setExp(int e) const {
    softdouble x;
    x.v =
        (v & 0x800FFFFFFFFFFFFF) | ((uint_fast64_t)((e + 1023) & 0x7FF) << 52);
    return x;
  }

  // Fraction part
  inline softdouble getFrac() const {
    uint_fast64_t vv = (v & 0x000FFFFFFFFFFFFF) | ((uint_fast64_t)(1023) << 52);
    return softdouble::fromRaw(vv);
  }
  inline softdouble setFrac(const softdouble& s) const {
    softdouble x;
    x.v = (v & 0xFFF0000000000000) | (s.v & 0x000FFFFFFFFFFFFF);
    return x;
  }

  // Default constants
  static softdouble zero() { return softdouble::fromRaw(0); }
  static softdouble inf() {
    return softdouble::fromRaw((uint_fast64_t)(0x7FF) << 52);
  }
  static softdouble nan() {
    return softdouble::fromRaw((uint_fast64_t)(0x7FFFFFFFFFFFFFFF));
  }
  static softdouble one() {
    return softdouble::fromRaw((uint_fast64_t)(1023) << 52);
  }
  static softdouble min() {
    return softdouble::fromRaw((uint_fast64_t)(0x01) << 52);
  }
  static softdouble max() {
    return softdouble::fromRaw(((uint_fast64_t)(0x7FF) << 52) - 1);
  }
  static softdouble eps() {
    return softdouble::fromRaw((uint_fast64_t)(1023 - 52) << 52);
  }

  uint64_t v;
};

// Arithmetics
softdouble sqrt(const softdouble& a);

// Other functions
inline softdouble min(const softdouble& a, const softdouble& b) {
  return (a > b) ? b : a;
}
inline softdouble max(const softdouble& a, const softdouble& b) {
  return (a > b) ? a : b;
}
inline softdouble abs(softdouble a) {
  softdouble x;
  x.v = a.v & ((1ULL << 63) - 1);
  return x;
}

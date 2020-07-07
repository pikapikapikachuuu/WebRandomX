#include <stdio.h>

#include "softfloat.hpp"

int main() {
  softdouble a(1.5), b(2.5);
  softdouble c = a + b;
  // printf("+11.5 -> %+4.1f\n", +11.5);
  printf("%lf\n", double(c));
  return 0;
}

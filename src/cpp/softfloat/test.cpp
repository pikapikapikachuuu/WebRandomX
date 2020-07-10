#include <stdio.h>

#include "softfloat.hpp"

int main() {
  softdouble a(1.5), b(2.5);
  printf("%lf\n", double(a + b));
  printf("%lf\n", double(a - b));
  printf("%lf\n", double(a * b));
  printf("%lf\n", double(a / b));
  printf("%lf\n", double(a % b));
  printf("%lf\n", double(sqrt(a)));
  return 0;
}


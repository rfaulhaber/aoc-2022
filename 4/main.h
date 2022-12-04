#ifndef MAIN_H_
#define MAIN_H_

#include "../utils/c/input.h"
#include <ctype.h>
#include <stdio.h>

typedef struct range {
  unsigned int min;
  unsigned int max;
} Range;

typedef struct range_pair {
  Range left;
  Range right;
} RangePair;

#endif // MAIN_H_

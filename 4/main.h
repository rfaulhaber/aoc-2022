#ifndef MAIN_H_
#define MAIN_H_

#include <stdio.h>

typedef struct range {
  unsigned int min;
  unsigned int max;
} Range;

typedef struct range_pair {
  Range left;
  Range right;
} RangePair;

Range get_range_from_line(char *line);

RangePair *make_range_pairs_from_input(char **contents, size_t file_length,
                                       size_t *range_length);

#endif // MAIN_H_

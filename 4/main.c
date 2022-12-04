#include "main.h"

int is_strict_overlap(RangePair pair) {
  /*
   * Overlap can be either:
   * .2345678.  2-8
   * ..34567..  3-7

   * .....6...  6-6
   * ...456...  4-6
   */
  int left_min, left_max, right_min, right_max;

  left_min = pair.left.min;
  left_max = pair.left.max;

  right_min = pair.right.min;
  right_max = pair.right.max;

  /* I know this isn't efficient (don't @ me) but we have to check it from both
   * sides */
  return (left_min <= right_min && left_max >= right_max) ||
         (left_min >= right_min && left_max <= right_max);
}

int is_overlap(RangePair pair) {
  int left_min, left_max, right_min, right_max;

  left_min = pair.left.min;
  left_max = pair.left.max;

  right_min = pair.right.min;
  right_max = pair.right.max;

  return left_min <= right_max && right_min <= left_max;
}

Range get_range_from_line(char *line) {
  size_t range_count;
  char **split = split_str(line, '-', &range_count);

  int low, high;

  sscanf(split[0], "%d", &low);
  sscanf(split[1], "%d", &high);

  Range r = {low, high};

  return r;
}

RangePair *make_range_pairs_from_input(char **contents, size_t file_length,
                                       size_t *range_length) {
  RangePair *pairs = malloc(file_length * sizeof(RangePair));

  for (size_t i = 0; i < file_length; i++) {
    size_t range_count;
    char **ranges = split_str(contents[i], ',', &range_count);

    /* dangerous! */
    Range left = get_range_from_line(ranges[0]);
    Range right = get_range_from_line(ranges[1]);

    RangePair pair = {left, right};
    pairs[i] = pair;
  }

  *range_length = file_length;

  return pairs;
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    char *path = argv[1];

    size_t file_length;
    char **contents = get_file_contents(path, &file_length);

    printf("file length: %zu\n", file_length);

    size_t range_length;
    RangePair *pairs =
        make_range_pairs_from_input(contents, file_length, &range_length);

    int part_1_overlaps = 0;
    int part_2_overlaps = 0;

    for (int i = 0; i < range_length; i++) {
      part_1_overlaps += is_strict_overlap(pairs[i]);
      part_2_overlaps += is_overlap(pairs[i]);
    }

    printf("part 1: %d\n", part_1_overlaps);
    printf("part 2: %d\n", part_2_overlaps);

    free(contents);
    free(pairs);
  }
}

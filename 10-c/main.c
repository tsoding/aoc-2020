#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define N 1000

unsigned long xs[N + 10];
int xs_size = 0;
unsigned long dp[N + 10];

int compare_int(const void *x, const void *y)
{
    return *(const unsigned long *)x - *(const unsigned long *)y;
}

unsigned long part_1(void)
{
    int one = 0;
    int three = 0;
    for (int i = 1; i < xs_size; ++i) {
        int d = xs[i] - xs[i - 1];
        switch (d) {
        case 1:
            one += 1;
            break;
        case 3:
            three += 1;
            break;
        default:
            assert(0 && "unreachable");
        }
    }

    return one * three;
}

unsigned long part_2(void)
{
    dp[0] = 1;
    for (int i = 1; i < xs_size; ++i) {
        dp[i] = 0;
        for (int j = i - 1; j >= 0 && xs[i] - xs[j] <= 3; --j) {
            dp[i] += dp[j];
        }
    }

    return dp[xs_size - 1];
}

void solve_file(const char *file_path)
{
    printf("Input file: %s\n", file_path);

    xs_size = 0;

    FILE *f = fopen(file_path, "r");
    xs[xs_size++] = 0;
    while (!feof(f)) {
        unsigned long x = 0;
        int n = fscanf(f, "%lu", &x);
        if (n == 1) {
            xs[xs_size++] = x;
        }
    }
    fclose(f);
    qsort(xs, xs_size, sizeof(xs[0]), compare_int);
    xs[xs_size] = xs[xs_size - 1] + 3;
    xs_size += 1;

    printf("Part 1: %lu\n", part_1());
    printf("Part 2: %lu\n", part_2());
}

int main(int argc, char *argv[])
{
    for (int i = 1; i < argc; ++i) {
        solve_file(argv[i]);
    }

    return 0;
}

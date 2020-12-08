import java.io.*;
import java.util.*;
import java.util.stream.*;

public class Main {
    static long countTrees(String[] input, int dy, int dx) {
        final int height = input.length;
        final int width = input[0].length();

        int y = dy;
        int x = dx % width;
        long result = 0;

        while (y < height) {
            if (input[y].charAt(x) == '#') {
                result += 1;
            }
            y = y + dy;
            x = (x + dx) % width;
        }

        return result;
    }

    static long part1(String[] input) {
        return countTrees(input, 1, 3);
    }

    static long part2(String[] input) {
        int slopes[][] = {
            {1, 1},
            {1, 3},
            {1, 5},
            {1, 7},
            {2, 1},
        };

        long result = 1;
        for (int i = 0; i < slopes.length; ++i) {
            result *= countTrees(input, slopes[i][0], slopes[i][1]);
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        if (args.length <= 0) {
            System.err.println("Input file is not provided");
            System.exit(1);
        }

        var filepath = args[0];

        String[] input = null;
        try(var file = new BufferedReader(new FileReader(filepath))) {
            input = file
                .lines()
                .collect(Collectors.toList())
                .toArray(new String[0]);
        }

        System.out.printf("Input file: %s\n", filepath);
        System.out.printf("Part 1: %d\n", part1(input));
        System.out.printf("Part 2: %d\n", part2(input));
    }
}

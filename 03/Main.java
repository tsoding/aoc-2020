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

    public static void main(String[] args) throws IOException {
        String[] input = null;
        try(var file = new BufferedReader(new FileReader("input.txt"))) {
            input = file
                .lines()
                .collect(Collectors.toList())
                .toArray(new String[0]);
        }

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
        System.out.println(result);
    }
}

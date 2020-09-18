import java.io.*;
import java.util.*;

public class TextSwap {

    private static String readFile(String filename) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        Interval[] intervals = new Interval[numChunks];
        int start = 0;
        for (int i = 0; i < numChunks; i++) {
            start = i * chunkSize;
            intervals[i] = new Interval(start, (start + chunkSize));
        }
        return intervals;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        // System.out.println(labels);
        return labels;
    }

    private static void reorderIntervals(Interval[] intervals, List<Character> labels) {
        for (int i = 0; i < intervals.length; i++) {
            Interval startInterval = intervals[i];
            int j = i;
            while (true) {
                int k = labels.get(j) - 'a';
                labels.set(j, (char) ('a' + j));
                if (k == i) {
                    break;
                }
                intervals[j] = intervals[k];
                j = k;
            }
            intervals[j] = startInterval;
        }
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) throws InterruptedException {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        char[] buffer = new char[chunkSize * numChunks];

        reorderIntervals(intervals, labels);

        // Run the swapper threads
        for (int i = 0; i < intervals.length; i++) {
            Swapper s = new Swapper(intervals[i], content, buffer, i * chunkSize);
            Thread t = new Thread(s);
            t.join();
            t.start();
        }

        return buffer;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, numChunks);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }

        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        int numChunks;

        try {
            contents = readFile(args[1]);
            numChunks = contents.length() / chunkSize;

            if (numChunks > 26) {
                System.err.println("Chunk size too small.");
                return;
            }

            if (numChunks <= 0 || contents.length() % chunkSize != 0) {
                System.err.println("Chunk size must be positive and file sizemust be a multiple of the chunk size");
                return;
            }

            writeToFile(contents, chunkSize, numChunks);

        } catch (Exception e) {
            System.out.println("Error with IO.");
            return;
        }
    }
}
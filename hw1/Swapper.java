public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        int chunkSize = interval.getY() - interval.getX();
        int bufferIndex = interval.getX();
        for (int i = offset; i < chunkSize + offset; i++) {
            buffer[i] = content.charAt(bufferIndex++);
        }
    }
}
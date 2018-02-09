public class Example {

    private static class Node {
        String elt;
        public Node() {}
    }

    public static void checkGuardedBy(boolean condition) {
        if (!condition) {
            throw new IllegalStateException("Bad");
        }
    }

    public static String visitMethodInvocationUnsafe(Node n) {
        return n.elt;
    }

    public static String visitMethodInvocationSafe(Node n) {
        checkGuardedBy(n != null);
        return n.elt;
    }


    public static void main(String[] args) {
        Node nSafe = new Node (); // Used to be null, but doesn't mean to be
        Node nUnsafe = null;
        String res;
        res = visitMethodInvocationUnsafe(nUnsafe);
        System.out.println(res);
        res = visitMethodInvocationSafe(nSafe);
        System.out.println(res);

    }
}

class Functor<> {
  public fmap(Function<A, B> f, ...) {
  }
}

class IntNode {
  public IntNode next;
  public int i;

  public IntNode(int v, IntNode n) {
    this.next = n;
    this.i = v;
  }
  public IntNode(int v) {
    this(v, null);
  }
}

public class FreeM {

  public static void walk(IntNode node) {
    if (node != null) {
      System.out.println(node.i);
      walk(node.next);
    }
  }

  public static void main(String[] args) {
    IntNode intList1 = new IntNode(1, new IntNode(2, new IntNode(3)));
    walk(intList1);
  }
}

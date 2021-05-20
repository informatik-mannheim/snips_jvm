public class Foo {
  public static void main(String[] args) {
    int a = 1;
    // +OUT
    // This will be excluded.
    System.out.println("Hello Word!");
    // +EXC
    // This is the solution:
    System.out.println("Value is " + a);
    // -EXC
    // -OUT
  }
}
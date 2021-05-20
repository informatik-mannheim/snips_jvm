public class Foo {
  public static void main(String[] args) {
    int a = 1;
    // +OUT
    // +EXCSUBST 4 // Your solution:
    // This is the solution:
    System.out.println("Value is " + a);
    // -EXCSUBST
    // -OUT
  }
}
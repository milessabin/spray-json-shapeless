import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.io.Closeable;
import java.util.Arrays;
import java.util.ArrayList;

@Retention(value=RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE_USE, ElementType.PARAMETER, ElementType.METHOD, ElementType.TYPE})
@interface MyAnnotation {}

@MyAnnotation
@Deprecated
class Test<@MyAnnotation X> {

  static void closures() {
      Arrays.sort(new Float[0], Float::compare);
      Arrays.sort(new Integer[0], Integer::compareTo);
      Arrays.sort(new Integer[0], (Integer a, Integer b) -> a.compareTo(b));
  }

  @Deprecated
  public int field1;

  @MyAnnotation
  public int field2;

  @Deprecated
  static void test(@MyAnnotation int param1) {
    try {
      new @MyAnnotation Test();
      test(param1);
    } catch (@MyAnnotation Exception e) {
      System.out.println(MyAnnotation.class.getName());
    }
  }

  final class InnerClassWithCtorParam {
    public InnerClassWithCtorParam(@MyAnnotation X param) {
      System.out.println(Test.this);
      System.out.println(param);
    }
  }
}

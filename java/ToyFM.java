import java.io.*;
import java.util.function.*;

// WTF, really? https://medium.com/@johnmcclean/dsls-with-the-free-monad-in-java-8-part-i-701408e874f8
// https://github.com/xuwei-k/free-monad-java
// https://dzone.com/articles/functor-and-monad-examples-in-plain-java

interface Functor<T, F extends Functor<?,?>> {
  <R> F map(Function<? super T, ? extends R> mapper);
}

abstract class Toy<T> implements Functor<T, Toy<?>> {
  public Toy<T> next;
  public Toy(Toy<T> next) { this.next = next; }

  @Override
  public <R> Toy<R> map(Function<? super T, ? extends R> mapper) { return null; }
}

class Output<T> extends Toy<T> {
  public T b;
  public Output(T item, Toy<T> next) { super(next); this.b = item; }
  @Override
  public <R> Output<R> map(Function<? super T, ? extends R> mapper) {
    return new Output<>(mapper.apply(this.b), this.next != null ? this.next.map(mapper) : null);
  }
}

class Bell<T> extends Toy<T> {
  public Bell(Toy<T> next) { super(next); }
  public <R> Bell<R> map(Function<? super T, ? extends R> mapper) {
    return new Bell<>(this.next != null ? this.next.map(mapper) : null);
  }
  
}

class Done<T> extends Toy<T> {
  public Done() { super(null); }
  public <R> Done<R> map(Function<? super T, ? extends R> mapper) {
    return new Done<R>();
  }
}

interface Monad<T, M extends Monad<?, ?>> extends Functor<T, M> {
  M flatMap(Function<T, M> f);
}

abstract class FOptional<T> implements Monad<T, FOptional<?>> {

    private final T valueOrNull;

    private FOptional(T valueOrNull) {
        this.valueOrNull = valueOrNull;
    }

    public <R> FOptional<R> map(Function<? super T, ? extends R> f) {
        if (valueOrNull == null)
            return empty();
        else
            return of(f.apply(valueOrNull));
    }

    public static <T> FOptional<T> of(T a) {
        return new FOptional<T>(a);
    }

    public static <T> FOptional<T> empty() {
        return new FOptional<T>(null);
    }
 
    public FOptional<T> flatMap(Function<T, M> f) {
        return map(f);
    }
}


class Printer<T> {
  public void print(Done<T> done) {
    System.out.println("Done");
  }
  public void print(Bell<T> bell) {
    System.out.println("Bell");
  }
  public void print(Output<T> output) {
    System.out.println(output.b);
  }
}

public class ToyFM {

  static Printer<Character> printer = new Printer<>();

  public static void walk(Toy<Character> toy) {
    toy.map(c -> { System.out.println(c); return c; });
  }
 
  public static void main(String[] args) {
    Toy<Character> toy = new Output('B', new Bell(new Output('A', new Done())));
    walk(toy);
  }
}

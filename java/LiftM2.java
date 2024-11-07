import java.io.*;
import java.util.function.*;

import java.time.LocalDate;
import java.time.Month;


// https://dzone.com/articles/functor-and-monad-examples-in-plain-java

interface Functor<T, F extends Functor<?,?>> {
  <R> F map(Function<? super T, ? extends R> mapper);
}

interface Monad<T, M extends Monad<?, ?>> extends Functor<T, M> {
  M flatMap(Function<T, M> f);
}

class MOptional<T> implements Monad<T, MOptional<?>> {

    private final T valueOrNull;

    private MOptional(T valueOrNull) {
        this.valueOrNull = valueOrNull;
    }

    public <R> MOptional<R> map(Function<? super T, ? extends R> f) {
        if (valueOrNull == null)
            return empty();
        else
            return of(f.apply(valueOrNull));
    }

    public static <T> MOptional<T> of(T a) {
        return new MOptional<T>(a);
    }

    public static <T> MOptional<T> empty() {
        return new MOptional<T>(null);
    }

    public <R> MOptional<R> flatMap(Function<T, MOptional<R>> f) {
        if (valueOrNull == null) {
            return empty();
        } else {
            return f.apply(valueOrNull);
        }
    }
}


public class LiftM2 {

  public static void work() {

  }
 
  public static void main(String[] args) {
    work();
  }
}

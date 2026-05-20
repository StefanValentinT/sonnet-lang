package util;

import java.util.Iterator;
import java.util.Objects;
import java.util.Spliterators;
import java.util.function.BiConsumer;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public final class Zipper {

	private Zipper() {
	}

	public static <A, B> void zip(Stream<A> a, Stream<B> b, BiConsumer<A, B> consumer) {
		Objects.requireNonNull(a);
		Objects.requireNonNull(b);
		Objects.requireNonNull(consumer);

		try (a; b) {
			Iterator<A> iteratorA = a.iterator();
			Iterator<B> iteratorB = b.iterator();

			while (iteratorA.hasNext() && iteratorB.hasNext()) {
				consumer.accept(iteratorA.next(), iteratorB.next());
			}

			if (iteratorA.hasNext() || iteratorB.hasNext()) {
				throw new IllegalArgumentException("Streams must be of equal length to zip");
			}
		}
	}
}
package cn.blmdz.wolf.common.utils;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;

public class Iters {
	public static <I> Iterable<I> nullToEmpty(@Nullable Iterable<I> iter) {
		return iter == null ? new Iterable<I>() {
			public Iterator<I> iterator() {
				return Collections.emptyIterator();
			}
		} : iter;
	}

	public static <I> List<I> nullToEmpty(@Nullable List<I> list) {
		return list == null ? Collections.<I> emptyList() : list;
	}

	public static <K, V> Map<K, V> nullToEmpty(@Nullable Map<K, V> map) {
		return map == null ? Collections.<K, V> emptyMap() : map;
	}

	public static <I> Iterable<I> emptyToNull(@Nullable Iterable<I> iter) {
		return iter != null && iter.iterator().hasNext() ? iter : null;
	}

	public static <I> List<I> emptyToNull(@Nullable List<I> list) {
		return list != null && !list.isEmpty() ? list : null;
	}

	public static <T> T[] emptyToNull(@Nullable T[] array) {
		return array != null && array.length > 0 ? array : null;
	}

	public static long[] emptyToNull(@Nullable long[] array) {
		return array != null && array.length > 0 ? array : null;
	}
}

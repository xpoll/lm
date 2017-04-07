package cn.blmdz.wolf.common.utils;

import javax.annotation.Nullable;

import com.google.common.base.Optional;

import cn.blmdz.home.common.util.Params;

public class Strs {
	public static Optional<Long> parseLong(@Nullable Object obj) {
		String str = Params.trimToNull(obj);
		return (str == null) ? Optional.<Long>absent() : Optional.of(Long.valueOf(Long.parseLong(str)));
	}

	public static Optional<Integer> parseInt(@Nullable Object obj) {
		String str = Params.trimToNull(obj);
		return (str == null) ? Optional.<Integer>absent() : Optional.of(Integer.valueOf(Integer.parseInt(str)));
	}
}
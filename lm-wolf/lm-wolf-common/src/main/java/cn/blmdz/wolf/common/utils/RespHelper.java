package cn.blmdz.wolf.common.utils;

import java.util.Collections;

import com.google.common.base.Optional;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;

public class RespHelper {
	public static <T> T or(Response<T> resp, T failValue) {
		return resp.isSuccess() ? resp.getResult() : failValue;
	}

	public static Boolean orFalse(Response<Boolean> resp) {
		return (Boolean) or(resp, Boolean.FALSE);
	}

	public static <T> T or500(Response<T> resp) {
		if (resp.isSuccess()) {
			return resp.getResult();
		} else {
			throw new JsonResponseException(500, resp.getError());
		}
	}

	public static <T> T orServEx(Response<T> resp) {
		if (resp.isSuccess()) {
			return resp.getResult();
		} else {
			throw new ServiceException(resp.getError());
		}
	}

	public static <T, D extends T> Response<T> ok(T data) {
		Response<T> resp = new Response<T>();
		resp.setResult(data);
		return resp;
	}

	public static final class Map {
		public static <K, V> Response<java.util.Map<Object, Object>> empty() {
			return Response.ok(Collections.emptyMap());
		}
	}

	public static final class Opt {
		public static <T> Response<T> unwrap(Response<Optional<T>> resp, String error) {
			if (resp.isSuccess()) {
				if (resp.getResult().isPresent()) {
					return Response.ok(resp.getResult().get());
				}
				return Response.fail(error);
			}
			return Response.fail(resp.getError());
		}

//		public static <T, D extends T> Response<Optional<T>> of(D data) {
//			return Response.ok(Optional.of(data));
//		}
//
//		public static <T> Response<Optional<T>> absent() {
//			return Response.ok(Optional.absent());
//		}
//
//		public static <T, D extends T> Response<Optional<T>> fromNullable(D data) {
//			return Response.ok(Optional.fromNullable(data));
//		}
	}
}

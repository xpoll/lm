package cn.blmdz.wolf.common.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;

public class RespUtil {
	private static final Logger log = LoggerFactory.getLogger(RespUtil.class);

	public static <T> T or(Response<T> resp, T failValue) {
		return (resp.isSuccess()) ? resp.getResult() : failValue;
	}

	public static Boolean orFalse(Response<Boolean> resp) {
		return (Boolean) or(resp, Boolean.FALSE);
	}

	public static <T> T orJsonEx(Response<T> resp, String method, Object[] params) {
		if (resp.isSuccess()) {
			return resp.getResult();
		}
		log.error("{} fail, cause=[{}], params=[{}]", new Object[] { method, resp.getError(), params });
		throw new JsonResponseException(500, resp.getError());
	}

	public static <T> T orServerEx(Response<T> resp, String method, Object[] params) {
		if (resp.isSuccess()) {
			return resp.getResult();
		}
		log.error("{} fail, cause=[{}], params=[{}]", new Object[] { method, params, resp.getError() });
		throw new ServiceException(resp.getError());
	}

	public static <T, D extends T> Response<T> ok(D data) {
		Response resp = new Response();
		resp.setResult(data);
		return resp;
	}
}
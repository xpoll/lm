package io.terminus.parana.common.util;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RespUtil {
   private static final Logger log = LoggerFactory.getLogger(RespUtil.class);

   public static Object or(Response resp, Object failValue) {
      return resp.isSuccess()?resp.getResult():failValue;
   }

   public static Boolean orFalse(Response resp) {
      return (Boolean)or(resp, Boolean.FALSE);
   }

   public static Object orJsonEx(Response resp, String method, Object... params) {
      if(resp.isSuccess()) {
         return resp.getResult();
      } else {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, resp.getError(), params});
         throw new JsonResponseException(500, resp.getError());
      }
   }

   public static Object orServerEx(Response resp, String method, Object... params) {
      if(resp.isSuccess()) {
         return resp.getResult();
      } else {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, params, resp.getError()});
         throw new ServiceException(resp.getError());
      }
   }

   public static Response ok(Object data) {
      Response resp = new Response();
      resp.setResult(data);
      return resp;
   }
}

package io.terminus.parana.common.util;

import com.google.common.base.CaseFormat;
import com.google.common.base.Throwables;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExUtil {
   private static final Logger log = LoggerFactory.getLogger(ExUtil.class);

   public static Response logResp(Exception e, String method, Object... params) {
      log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, Throwables.getStackTraceAsString(e), params});
      String errorCode = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, method);
      errorCode = errorCode.replace("_", ".") + ".fail";
      return Response.fail(errorCode);
   }

   public static JsonResponseException logJsonEx(Exception e, String method, Object... params) {
      if(e instanceof JsonResponseException) {
         return (JsonResponseException)e;
      } else {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, Throwables.getStackTraceAsString(e), params});
         String errorCode = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, method);
         errorCode = errorCode.replace("_", ".") + ".fail";
         return new JsonResponseException(errorCode);
      }
   }

   public static ServiceException logSrvEx(Exception e, String method, Object... params) {
      if(e instanceof ServiceException) {
         return (ServiceException)e;
      } else {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, Throwables.getStackTraceAsString(e), params});
         String errorCode = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, method);
         errorCode = errorCode.replace("_", ".") + ".fail";
         return new ServiceException(errorCode);
      }
   }
}

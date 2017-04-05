package io.terminus.parana.common.utils;

import com.google.common.base.Optional;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import java.util.Collections;

public class RespHelper {
   public static Object or(Response resp, Object failValue) {
      return resp.isSuccess()?resp.getResult():failValue;
   }

   public static Boolean orFalse(Response resp) {
      return (Boolean)or(resp, Boolean.FALSE);
   }

   public static Object or500(Response resp) {
      if(resp.isSuccess()) {
         return resp.getResult();
      } else {
         throw new JsonResponseException(500, resp.getError());
      }
   }

   public static Object orServEx(Response resp) {
      if(resp.isSuccess()) {
         return resp.getResult();
      } else {
         throw new ServiceException(resp.getError());
      }
   }

   public static Response ok(Object data) {
      Response<T> resp = new Response();
      resp.setResult(data);
      return resp;
   }

   public static final class Map {
      public static Response empty() {
         return Response.ok(Collections.emptyMap());
      }
   }

   public static final class Opt {
      public static Response unwrap(Response resp, String error) {
         return resp.isSuccess()?(((Optional)resp.getResult()).isPresent()?Response.ok(((Optional)resp.getResult()).get()):Response.fail(error)):Response.fail(resp.getError());
      }

      public static Response of(Object data) {
         return Response.ok(Optional.of(data));
      }

      public static Response absent() {
         return Response.ok(Optional.absent());
      }

      public static Response fromNullable(Object data) {
         return Response.ok(Optional.fromNullable(data));
      }
   }
}

package io.terminus.parana.common.util;

import io.terminus.common.exception.ServiceException;
import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SrvCheck {
   private static final Logger log = LoggerFactory.getLogger(SrvCheck.class);

   public static void checkArgument(boolean expression, @Nullable String messageCode, @Nullable String method, @Nullable Object... errorMessageArgs) {
      if(!expression) {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, messageCode, errorMessageArgs});
         throw new ServiceException(messageCode);
      }
   }

   public static void checkState(boolean expression, @Nullable String messageCode, @Nullable String method, @Nullable Object... errorMessageArgs) {
      if(!expression) {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, messageCode, errorMessageArgs});
         throw new ServiceException(messageCode);
      }
   }

   public static Object checkNotNull(Object reference, @Nullable String messageCode, @Nullable String method, @Nullable Object... errorMessageArgs) {
      if(reference == null) {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, messageCode, errorMessageArgs});
         throw new ServiceException(messageCode);
      } else {
         return reference;
      }
   }

   public static void checkListNotEmpty(List reference, @Nullable String messageCode, @Nullable String method, @Nullable Object... errorMessageArgs) {
      if(reference == null || reference.isEmpty()) {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, messageCode, errorMessageArgs});
         throw new ServiceException(messageCode);
      }
   }

   public static void checkMapNotEmpty(Map reference, @Nullable String messageCode, @Nullable String method, @Nullable Object... errorMessageArgs) {
      if(reference == null || reference.isEmpty()) {
         log.error("{} fail, cause=[{}], params=[{}]", new Object[]{method, messageCode, errorMessageArgs});
         throw new ServiceException(messageCode);
      }
   }
}

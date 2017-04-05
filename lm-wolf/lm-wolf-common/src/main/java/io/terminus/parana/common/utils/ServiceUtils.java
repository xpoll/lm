package io.terminus.parana.common.utils;

import com.google.common.base.Optional;
import io.terminus.common.exception.ServiceException;
import javax.annotation.Nullable;

public class ServiceUtils {
   public static Object throwz(String error) {
      throw new ServiceException(String.valueOf(error));
   }

   public static ServiceException error(String error) {
      return new ServiceException(String.valueOf(error));
   }

   public static void checkResult(boolean expression, @Nullable String error) {
      if(!expression) {
         throw new ServiceException(String.valueOf(error));
      }
   }

   public static Object getResult(Optional optional, @Nullable String error) {
      return unwrap(optional, error);
   }

   public static Object unwrap(Optional optional, @Nullable String error) {
      if(optional.isPresent()) {
         return optional.get();
      } else {
         throw new ServiceException(String.valueOf(error));
      }
   }
}

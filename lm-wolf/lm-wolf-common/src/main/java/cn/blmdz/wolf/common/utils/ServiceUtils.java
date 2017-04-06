package cn.blmdz.wolf.common.utils;

import javax.annotation.Nullable;

import com.google.common.base.Optional;

import cn.blmdz.home.common.exception.ServiceException;

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

package io.terminus.parana.auth.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.springframework.util.ReflectionUtils;

public final class ReflectUtil {
   public static Object invokeStaticMethod(Method method) throws InvocationTargetException, IllegalAccessException {
      return method.invoke((Object)null, (Object[])null);
   }

   public static Method findMethod(Class clazz, String methodName) {
      return ReflectionUtils.findMethod(clazz, methodName);
   }
}

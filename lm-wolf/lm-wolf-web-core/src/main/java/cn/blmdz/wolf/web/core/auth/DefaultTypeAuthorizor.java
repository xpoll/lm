package cn.blmdz.wolf.web.core.auth;

import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import cn.blmdz.wolf.web.core.auth.TypeAuthorizor;

public class DefaultTypeAuthorizor implements TypeAuthorizor {
   private static final Logger log = LoggerFactory.getLogger(DefaultTypeAuthorizor.class);

   public boolean matches(Set expectedTypes, Integer actualType) {
      if(!expectedTypes.contains("ALL") && !expectedTypes.contains("USER")) {
         switch(actualType.intValue()) {
         case 0:
            if(log.isDebugEnabled()) {
               log.debug("expected type: {}, actual type:{} ", expectedTypes, actualType);
            }

            return expectedTypes.contains("ADMIN");
         case 1:
            if(log.isDebugEnabled()) {
               log.debug("expected type: {}, actual type:{} ", expectedTypes, actualType);
            }

            return expectedTypes.contains("BUYER");
         case 2:
            if(log.isDebugEnabled()) {
               log.debug("expected type: {}, actual type:{} ", expectedTypes, actualType);
            }

            return expectedTypes.contains("SELLER");
         default:
            return false;
         }
      } else {
         return true;
      }
   }
}

package io.terminus.parana.web.core.auth;

import io.terminus.parana.web.core.auth.RoleAuthorizor;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DefaultRoleAuthorizor implements RoleAuthorizor {
   private static final Logger log = LoggerFactory.getLogger(DefaultRoleAuthorizor.class);

   public boolean matches(Set expectedRoles, List actualRoles) {
      if(CollectionUtils.isEmpty(expectedRoles)) {
         return true;
      } else if(CollectionUtils.isEmpty(actualRoles)) {
         return false;
      } else {
         for(String expectedRole : expectedRoles) {
            if(actualRoles.contains(expectedRole)) {
               return true;
            }
         }

         if(log.isDebugEnabled()) {
            log.debug("expected roles: {}, actual roles:{} ", expectedRoles, actualRoles);
         }

         return false;
      }
   }
}

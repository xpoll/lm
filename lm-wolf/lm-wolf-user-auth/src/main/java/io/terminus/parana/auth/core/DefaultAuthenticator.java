package io.terminus.parana.auth.core;

import com.google.common.base.Strings;
import io.terminus.parana.auth.core.AclLoader;
import io.terminus.parana.auth.core.Authenticator;
import io.terminus.parana.auth.core.PermissionHelper;
import io.terminus.parana.auth.model.Acl;
import io.terminus.parana.auth.model.ParanaThreadVars;
import io.terminus.parana.auth.model.PermissionData;
import io.terminus.parana.auth.model.Req;
import io.terminus.parana.auth.model.Request;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.common.utils.Iters;
import java.util.List;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DefaultAuthenticator implements Authenticator {
   private static final Logger log = LoggerFactory.getLogger(DefaultAuthenticator.class);
   private final AclLoader aclLoader;
   private final PermissionHelper permissionHelper;
   private final String level;

   public DefaultAuthenticator(AclLoader aclLoader, PermissionHelper permissionHelper, String level) {
      this.aclLoader = aclLoader;
      this.permissionHelper = permissionHelper;
      this.level = level;
   }

   public boolean ask(ParanaUser user, Req req) {
      Acl acl = this.aclLoader.getAcl(ParanaThreadVars.getApp());
      PermissionData perm = this.permissionHelper.getPermissions(acl, user);
      return this.checkMatch(perm.getRequests(), req)?true:(!"easy".equalsIgnoreCase(this.level)?false:!this.checkMatch(perm.getAllRequests(), req));
   }

   public boolean ask(ParanaUser user, String key) {
      Acl acl = this.aclLoader.getAcl(ParanaThreadVars.getApp());
      PermissionData perm = this.permissionHelper.getPermissions(acl, user);
      return perm.getResources().contains(key);
   }

   private boolean checkMatch(List defs, Req req) {
      for(Request def : Iters.nullToEmpty(defs)) {
         if(this.checkMatch(def, req)) {
            return true;
         }
      }

      return false;
   }

   private boolean checkMatch(Request def, Req req) {
      String path;
      if("GET".equalsIgnoreCase(req.getMethod())) {
         path = def.getGet();
      } else if("POST".equalsIgnoreCase(req.getMethod())) {
         path = def.getPost();
      } else if("PUT".equalsIgnoreCase(req.getMethod())) {
         path = def.getPut();
      } else {
         if(!"DELETE".equalsIgnoreCase(req.getMethod())) {
            throw new RuntimeException("unsupported http method");
         }

         path = def.getDelete();
      }

      if(Strings.isNullOrEmpty(path)) {
         return false;
      } else {
         Pattern pat = Pattern.compile(path);
         return pat.matcher(req.getPath()).matches();
      }
   }
}

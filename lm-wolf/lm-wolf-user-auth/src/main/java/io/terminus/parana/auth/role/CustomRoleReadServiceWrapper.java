package io.terminus.parana.auth.role;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.auth.role.CustomRoleLoadException;
import io.terminus.parana.auth.role.CustomRoleLoader;
import io.terminus.parana.common.utils.Iters;
import io.terminus.parana.user.auth.CustomRole;
import io.terminus.parana.user.auth.CustomRoleReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CustomRoleReadServiceWrapper implements CustomRoleLoader {
   private static final Logger log = LoggerFactory.getLogger(CustomRoleReadServiceWrapper.class);
   private final CustomRoleReadService customRoleReadService;

   public CustomRoleReadServiceWrapper(CustomRoleReadService customRoleReadService) {
      this.customRoleReadService = customRoleReadService;
   }

   public List load(List ids) throws CustomRoleLoadException {
      try {
         Response<List<CustomRole>> resp = this.customRoleReadService.findByIds(ids);
         if(!resp.isSuccess()) {
            log.warn("load custom role failed, error={}", resp.getError());
            throw new CustomRoleLoadException("Load custom role failed, error=%s", new Object[]{resp.getError()});
         } else {
            return Iters.nullToEmpty((List)resp.getResult());
         }
      } catch (Exception var3) {
         Throwables.propagateIfInstanceOf(var3, CustomRoleLoadException.class);
         log.error("load custom role failed, ids={}, cause;{}", Throwables.getStackTraceAsString(var3));
         throw new CustomRoleLoadException("Load custom role failed, ids=%s", new Object[]{ids});
      }
   }
}

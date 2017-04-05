package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import java.util.Map;

public interface AdminUserService {
   Response updateTags(Long var1, Map var2);

   Response updateStatus(Long var1, Integer var2);
}

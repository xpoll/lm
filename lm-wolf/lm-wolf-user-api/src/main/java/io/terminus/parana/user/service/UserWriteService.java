package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import io.terminus.parana.user.model.User;

public interface UserWriteService {
   Response create(User var1);

   Response update(User var1);
}

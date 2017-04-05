package io.terminus.parana.web.core.util;

import io.terminus.common.utils.BeanMapper;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.user.model.User;

public abstract class ParanaUserMaker {
   public static final ParanaUser from(User user) {
      ParanaUser paranaUser = new ParanaUser();
      BeanMapper.copy(user, paranaUser);
      return paranaUser;
   }
}

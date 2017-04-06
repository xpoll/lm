package cn.blmdz.wolf.web.core.util;

import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.user.model.User;

public abstract class ParanaUserMaker {
   public static final ParanaUser from(User user) {
      ParanaUser paranaUser = new ParanaUser();
      BeanMapper.copy(user, paranaUser);
      return paranaUser;
   }
}

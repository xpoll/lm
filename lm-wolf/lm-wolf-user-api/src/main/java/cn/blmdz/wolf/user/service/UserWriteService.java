package cn.blmdz.wolf.user.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.model.User;

public interface UserWriteService<T extends User> {
   Response<Long> create(User var1);

   Response update(User var1);
}

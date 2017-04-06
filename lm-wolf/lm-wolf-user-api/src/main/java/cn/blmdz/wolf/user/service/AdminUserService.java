package cn.blmdz.wolf.user.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;

public interface AdminUserService {
   Response updateTags(Long var1, Map var2);

   Response updateStatus(Long var1, Integer var2);
}

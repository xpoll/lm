package cn.blmdz.wolf.user.auth;

import cn.blmdz.home.common.model.Response;

public interface UserRoleLoader {
   Response<RoleContent> hardLoadRoles(Long var1);
}

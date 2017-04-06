package cn.blmdz.wolf.web.core.auth;

import java.util.List;
import java.util.Set;

public interface RoleAuthorizor {
   boolean matches(Set<String> var1, List var2);
}

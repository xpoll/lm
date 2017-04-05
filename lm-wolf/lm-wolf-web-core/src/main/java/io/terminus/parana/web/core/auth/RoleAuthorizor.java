package io.terminus.parana.web.core.auth;

import java.util.List;
import java.util.Set;

public interface RoleAuthorizor {
   boolean matches(Set var1, List var2);
}

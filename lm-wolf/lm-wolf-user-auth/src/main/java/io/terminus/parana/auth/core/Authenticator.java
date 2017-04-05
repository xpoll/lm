package io.terminus.parana.auth.core;

import io.terminus.parana.auth.model.Req;
import io.terminus.parana.common.model.ParanaUser;

public interface Authenticator {
   boolean ask(ParanaUser var1, Req var2);

   boolean ask(ParanaUser var1, String var2);
}

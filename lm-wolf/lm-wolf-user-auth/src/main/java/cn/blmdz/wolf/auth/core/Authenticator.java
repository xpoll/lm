package cn.blmdz.wolf.auth.core;

import cn.blmdz.wolf.auth.model.Req;
import cn.blmdz.wolf.common.model.ParanaUser;

public interface Authenticator {
   boolean ask(ParanaUser var1, Req var2);

   boolean ask(ParanaUser var1, String var2);
}

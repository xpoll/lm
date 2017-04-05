package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import javax.annotation.Nullable;

public interface SubDomainWriteService {
   Response setSubDomain(String var1, int var2, long var3, @Nullable String var5);

   Response delete(long var1);
}

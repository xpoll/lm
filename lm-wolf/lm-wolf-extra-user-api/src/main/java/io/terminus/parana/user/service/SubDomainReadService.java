package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.user.model.SubDomainType;
import javax.annotation.Nullable;

public interface SubDomainReadService {
   Response get(long var1, SubDomainType var3);

   Response getAllowNotFound(long var1, SubDomainType var3);

   Response getByDomain(String var1, SubDomainType var2);

   Response getByDomainAllowNotFound(String var1, SubDomainType var2);

   Response getByType(SubDomainType var1);

   @Export(
      paramNames = {"type", "pageNo", "size"}
   )
   Response pagination(@Nullable Integer var1, Integer var2, Integer var3);
}

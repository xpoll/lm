package cn.blmdz.wolf.user.service;

import javax.annotation.Nullable;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.user.model.SubDomainType;

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

package cn.blmdz.wolf.user.service;

import javax.annotation.Nullable;

import cn.blmdz.home.common.model.Response;

public interface SubDomainWriteService {
   Response setSubDomain(String var1, int var2, long var3, @Nullable String var5);

   Response delete(long var1);
}

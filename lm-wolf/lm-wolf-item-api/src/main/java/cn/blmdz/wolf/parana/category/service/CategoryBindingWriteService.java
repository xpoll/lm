package cn.blmdz.wolf.parana.category.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;

public interface CategoryBindingWriteService {
   Response bind(Long var1, Long var2);

   Response multiBind(Long var1, List<Long> var2);

   Response unBind(Long var1, Long var2);

   Response multiUnBind(Long var1, List var2);
}

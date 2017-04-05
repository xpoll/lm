package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import java.util.List;

public interface CategoryBindingWriteService {
   Response bind(Long var1, Long var2);

   Response multiBind(Long var1, List var2);

   Response unBind(Long var1, Long var2);

   Response multiUnBind(Long var1, List var2);
}

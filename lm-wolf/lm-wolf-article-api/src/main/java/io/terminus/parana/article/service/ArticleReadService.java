package io.terminus.parana.article.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.article.model.Article;
import java.util.Map;

public interface ArticleReadService {
   @Export(
      paramNames = {"id"}
   )
   Response findById(Long var1);

   @Export(
      paramNames = {"pageNo", "pageSize", "criteria"}
   )
   Response paging(Integer var1, Integer var2, Map var3);

   Response listBy(Article var1);
}

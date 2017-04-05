package io.terminus.parana.article.service;

import io.terminus.common.model.Response;
import io.terminus.parana.article.model.Article;

public interface ArticleWriteService {
   Response create(Article var1);

   Response update(Article var1);

   Response delete(Long var1);

   Response setStatus(Long var1, Integer var2);
}

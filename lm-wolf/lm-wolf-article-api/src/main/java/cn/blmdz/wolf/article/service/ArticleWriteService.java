package cn.blmdz.wolf.article.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.article.model.Article;

public interface ArticleWriteService {
   Response create(Article var1);

   Response update(Article var1);

   Response delete(Long var1);

   Response setStatus(Long var1, Integer var2);
}

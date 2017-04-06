package cn.blmdz.wolf.article.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.article.model.Article;

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

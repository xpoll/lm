package cn.blmdz.wolf.article.impl.service;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.article.impl.dao.ArticleDao;
import cn.blmdz.wolf.article.model.Article;
import cn.blmdz.wolf.article.service.ArticleReadService;

@Service
public class ArticleReadServiceImpl implements ArticleReadService {
   private static final Logger log = LoggerFactory.getLogger(ArticleReadServiceImpl.class);
   @Autowired
   private ArticleDao articleDao;

   public Response findById(Long id) {
      try {
         if(id != null && id.longValue() > 0L) {
            return Response.ok(this.articleDao.findById(id));
         } else {
            log.warn("article id(id={}) invalid, no article exist", id);
            return Response.ok((Object)null);
         }
      } catch (Exception var3) {
         log.error("find article by id={} failed, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("article.find.fail");
      }
   }

   public Response<Paging<Article>> paging(Integer pageNo, Integer pageSize, Map criteria) {
      Response<Paging<Article>> resp = new Response<Paging<Article>>();

      try {
         PageInfo pageInfo = new PageInfo(pageNo, pageSize);
         resp.setResult(this.articleDao.paging(pageInfo.getOffset(), pageInfo.getLimit(), criteria));
      } catch (Exception var6) {
         log.error("failed to find article by criteria : ({}), cause:{}", criteria.toString(), Throwables.getStackTraceAsString(var6));
         resp.setResult(Paging.<Article>empty());
      }

      return resp;
   }

   public Response listBy(Article criteria) {
      try {
         return Response.ok(this.articleDao.list(criteria));
      } catch (Exception var3) {
         log.error("fail to list article by criteria:{}, cause:{}", criteria, Throwables.getStackTraceAsString(var3));
         return Response.fail("article.find.fail");
      }
   }

   public Response<Paging<Article>> paging(Integer pageNo, Integer pageSize) {
      Response<Paging<Article>> resp = new Response();

      try {
         resp.setResult(this.articleDao.paging(pageNo, pageSize));
      } catch (Exception var5) {
         log.error("failed to find article, cause:{}", Throwables.getStackTraceAsString(var5));
         resp.setResult(Paging.<Article>empty());
      }

      return resp;
   }
}

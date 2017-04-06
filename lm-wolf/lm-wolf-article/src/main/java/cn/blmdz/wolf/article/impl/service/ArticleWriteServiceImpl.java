package cn.blmdz.wolf.article.impl.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.article.impl.dao.ArticleDao;
import cn.blmdz.wolf.article.model.Article;
import cn.blmdz.wolf.article.service.ArticleWriteService;

@Service
public class ArticleWriteServiceImpl implements ArticleWriteService {
   private static final Logger log = LoggerFactory.getLogger(ArticleWriteServiceImpl.class);
   @Autowired
   private ArticleDao articleDao;

   public Response create(Article article) {
      try {
         if(article == null) {
            log.error("article null, cannot create");
            return Response.fail("article.invalid");
         } else {
            this.articleDao.create(article);
            return Response.ok(article.getId());
         }
      } catch (Exception var3) {
         log.error("create article failed, article={}, cause:{}", article, Throwables.getStackTraceAsString(var3));
         return Response.fail("article.create.fail");
      }
   }

   public Response update(Article article) {
      try {
         if(article != null && article.getId() != null) {
            return Response.ok(this.articleDao.update(article));
         } else {
            log.error("article null or id null, cannot update");
            return Response.fail("article.invalid");
         }
      } catch (Exception var3) {
         log.error("update article failed, article={}, cause:{}", article, Throwables.getStackTraceAsString(var3));
         return Response.fail("article.update.fail");
      }
   }

   public Response delete(Long id) {
      try {
         return Response.ok(this.articleDao.delete(id));
      } catch (Exception var3) {
         log.error("delete article failed, article id = {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("article.delete.fail");
      }
   }

   public Response setStatus(Long id, Integer status) {
      try {
         return Response.ok(Boolean.valueOf(this.articleDao.setStatus(id, status).intValue() > 0));
      } catch (Exception var4) {
         log.error("change status of article failed, article id = {}, status = {}, cause:{}", new Object[]{id, status, Throwables.getStackTraceAsString(var4)});
         return Response.fail("article.status.change.fail");
      }
   }
}

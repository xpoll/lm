package io.terminus.parana.category.impl.service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.parana.category.impl.dao.BackCategoryDao;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.category.service.BackCategoryReadService;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class BackCategoryReadServiceImpl implements BackCategoryReadService {
   private static final Logger log = LoggerFactory.getLogger(BackCategoryReadServiceImpl.class);
   @Autowired
   private BackCategoryDao backCategoryDao;

   public Response findById(Long id) {
      Long categoryId = (Long)MoreObjects.firstNonNull(id, Long.valueOf(0L));

      try {
         BackCategory backCategory = (BackCategory)this.backCategoryDao.findById(categoryId);
         if(backCategory == null) {
            log.error("no back category(categoryId={}) found", categoryId);
            return Response.fail("backCategory.not.found");
         } else {
            return Response.ok(backCategory);
         }
      } catch (Exception var4) {
         log.error("failed to find back category(id={}), cause:{}", categoryId, Throwables.getStackTraceAsString(var4));
         return Response.fail("category.find.fail");
      }
   }

   public Response findChildrenByPid(Long pid) {
      Long categoryId = (Long)MoreObjects.firstNonNull(pid, Long.valueOf(0L));

      try {
         List<BackCategory> children = this.backCategoryDao.findChildren(categoryId);
         return Response.ok(children);
      } catch (Exception var4) {
         log.error("failed to find children of back category(id={}), cause:{}", categoryId, Throwables.getStackTraceAsString(var4));
         return Response.fail("category.find.fail");
      }
   }

   public Response findAncestorsOf(Long id) {
      Long categoryId = (Long)MoreObjects.firstNonNull(id, Long.valueOf(0L));
      if(categoryId.longValue() == 0L) {
         return Response.ok(Collections.emptyList());
      } else {
         List<BackCategory> ancestors = Lists.newArrayList();
         Long currentId = categoryId;

         try {
            while(currentId.longValue() > 0L) {
               BackCategory current = (BackCategory)this.backCategoryDao.findById(currentId);
               if(current == null) {
                  log.error("no back category(categoryId={}) found", currentId);
                  return Response.fail("backCategory.not.found");
               }

               ancestors.add(current);
               currentId = current.getPid();
            }

            return Response.ok(Lists.reverse(ancestors));
         } catch (Exception var6) {
            log.error("failed to find ancestors of back category(id={}), cause:{}", categoryId, Throwables.getStackTraceAsString(var6));
            return Response.fail("category.find.fail");
         }
      }
   }
}

package cn.blmdz.wolf.cache;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.category.model.BackCategory;
import cn.blmdz.wolf.category.service.BackCategoryReadService;

@Component
public class BackCategoryCacher {
   private static final Logger log = LoggerFactory.getLogger(BackCategoryCacher.class);
   private final LoadingCache<Long, BackCategory> backCategoryCache;
   private final LoadingCache<Long, List<BackCategory>> childrenCache;

   @Autowired
   public BackCategoryCacher(final BackCategoryReadService backCategoryReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.backCategoryCache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, BackCategory>() {
         public BackCategory load(Long categoryId) throws Exception {
            Response<BackCategory> rBackCategory = backCategoryReadService.findById(categoryId);
            if(!rBackCategory.isSuccess()) {
               BackCategoryCacher.log.error("failed to find back category(id={}), error code:{}", categoryId, rBackCategory.getError());
               throw new ServiceException("find back category fail,code: " + rBackCategory.getError());
            } else {
               return (BackCategory)rBackCategory.getResult();
            }
         }
      });
      this.childrenCache = CacheBuilder.newBuilder().expireAfterAccess((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, List<BackCategory>>() {
         public List<BackCategory> load(Long categoryId) throws Exception {
            Response<List<BackCategory>> rBcChildren = backCategoryReadService.findChildrenByPid(categoryId);
            if(!rBcChildren.isSuccess()) {
               BackCategoryCacher.log.error("failed to find children of back category(id={}), error code:{}", categoryId, rBcChildren.getError());
               throw new ServiceException("find back category children fail,code: " + rBcChildren.getError());
            } else {
               return (List)rBcChildren.getResult();
            }
         }
      });
   }

   public BackCategory findBackCategoryById(Long categoryId) {
      return (BackCategory)this.backCategoryCache.getUnchecked(categoryId);
   }

   public List findAncestorsOf(Long id) {
      Long categoryId = (Long)MoreObjects.firstNonNull(id, Long.valueOf(0L));
      if(categoryId.longValue() == 0L) {
         return Collections.emptyList();
      } else {
         List<BackCategory> ancestors = Lists.newArrayList();
         Long currentId = categoryId;

         try {
            while(currentId.longValue() > 0L) {
               BackCategory current = (BackCategory)this.backCategoryCache.getUnchecked(currentId);
               if(current == null) {
                  log.error("no back category(categoryId={}) found", currentId);
                  throw new ServiceException("backCategory.not.found");
               }

               ancestors.add(current);
               currentId = current.getPid();
            }

            return Lists.reverse(ancestors);
         } catch (Exception var6) {
            log.error("failed to find ancestors of back category(id={}), cause:{}", categoryId, Throwables.getStackTraceAsString(var6));
            throw var6;
         }
      }
   }

   @Export(
      paramNames = {"id"}
   )
   public List findChildrenOf(Long id) {
      return (List)this.childrenCache.getUnchecked(id);
   }
}

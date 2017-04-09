package cn.blmdz.wolf.cache;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.model.BackCategory;
import cn.blmdz.wolf.category.service.CategoryBindingReadService;

@Component
public class CategoryBindingCacher {
   private static final Logger log = LoggerFactory.getLogger(CategoryBindingCacher.class);
   private final CategoryBindingReadService categoryBindingReadService;
   private final LoadingCache<Long, List<BackCategory>> bindingCache;

   @Autowired
   public CategoryBindingCacher(final CategoryBindingReadService categoryBindingReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.categoryBindingReadService = categoryBindingReadService;
      this.bindingCache = CacheBuilder.newBuilder().expireAfterAccess((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader<Long, List<BackCategory>>() {
         public List<BackCategory> load(Long frontCategoryId) throws Exception {
            Response<List<BackCategory>> r = categoryBindingReadService.findByFrontCategoryId(frontCategoryId);
            if(!r.isSuccess()) {
               CategoryBindingCacher.log.error("failed to find responding back categories for front category(id={}), error code:{}", frontCategoryId, r.getError());
               throw new ServiceException(r.getError());
            } else {
               return (List)r.getResult();
            }
         }
      });
   }

   public List findByFrontCategoryId(Long frontCategoryId) {
      List<BackCategory> backCategories = (List)this.bindingCache.getUnchecked(frontCategoryId);
      List<Long> backCategoryIds = Lists.newArrayListWithCapacity(backCategories.size());

      for(BackCategory backCategory : backCategories) {
         backCategoryIds.add(backCategory.getId());
      }

      return backCategoryIds;
   }

   public List findBindingByFrontCategoryId(Long frontCategoryId) {
      return (List)this.bindingCache.getUnchecked(frontCategoryId);
   }
}

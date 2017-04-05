package io.terminus.parana.cache;

import com.google.common.base.Function;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.category.dto.GroupedCategoryAttribute;
import io.terminus.parana.category.model.CategoryAttribute;
import io.terminus.parana.category.service.CategoryAttributeReadService;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class CategoryAttributeCacher {
   private static final Logger log = LoggerFactory.getLogger(CategoryAttributeCacher.class);
   private final LoadingCache cache;

   @Autowired
   public CategoryAttributeCacher(final CategoryAttributeReadService categoryAttributeReadService, @Value("${cache.duration: 60}") Integer duration) {
      this.cache = CacheBuilder.newBuilder().expireAfterWrite((long)duration.intValue(), TimeUnit.MINUTES).build(new CacheLoader() {
         public List load(Long categoryId) throws Exception {
            Response<List<CategoryAttribute>> result = categoryAttributeReadService.findByCategoryId(categoryId);
            if(result.isSuccess()) {
               return (List)result.getResult();
            } else {
               CategoryAttributeCacher.log.error("failed to find attributes for category(id={}), error code:{}", categoryId, result.getError());
               throw new ServiceException("find category attributes fail, code: " + result.getError());
            }
         }
      });
   }

   @Export(
      paramNames = {"categoryId"}
   )
   public List findByCategoryId(Long categoryId) {
      return (List)this.cache.getUnchecked(categoryId);
   }

   @Export(
      paramNames = {"categoryId"}
   )
   public List findGroupedAttributeByCategoryId(Long categoryId) {
      List<CategoryAttribute> categoryAttributes = (List)this.cache.getUnchecked(categoryId);
      Multimap<String, CategoryAttribute> byGroup = Multimaps.index(categoryAttributes, new Function() {
         public String apply(CategoryAttribute categoryAttribute) {
            return categoryAttribute.getGroup();
         }
      });
      List<GroupedCategoryAttribute> result = Lists.newArrayListWithCapacity(byGroup.keySet().size());

      for(String group : byGroup.keySet()) {
         GroupedCategoryAttribute gca = new GroupedCategoryAttribute();
         gca.setGroup(group);
         gca.setCategoryAttributes(byGroup.get(group));
         result.add(gca);
      }

      return result;
   }
}

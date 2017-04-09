package cn.blmdz.wolf.component.attribute;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.category.dto.GroupedCategoryAttribute;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.category.service.CategoryAttributeReadService;

@Component
public class CategoryAttributeNoCacher {
   private static final Logger log = LoggerFactory.getLogger(CategoryAttributeNoCacher.class);
   private final CategoryAttributeReadService categoryAttributeReadService;

   @Autowired
   public CategoryAttributeNoCacher(CategoryAttributeReadService categoryAttributeReadService) {
      this.categoryAttributeReadService = categoryAttributeReadService;
   }

   @Export(
      paramNames = {"categoryId"}
   )
   public List findCategoryAttributeByCategoryId(Long categoryId) {
      Response<List<CategoryAttribute>> r = this.categoryAttributeReadService.findByCategoryId(categoryId);
      if(!r.isSuccess()) {
         log.error("failed to find category attributes by category(id={}), error code:{}", categoryId, r.getError());
         throw new ServiceException(r.getError());
      } else {
         return (List)r.getResult();
      }
   }

   @Export(
      paramNames = {"categoryId"}
   )
   public List findGroupedAttributeByCategoryId(Long categoryId) {
      List<CategoryAttribute> categoryAttributes = this.findCategoryAttributeByCategoryId(categoryId);
      Multimap<String, CategoryAttribute> byGroup = Multimaps.index(categoryAttributes, new Function<CategoryAttribute, String>() {
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

package io.terminus.parana.rule.attribute;

import io.terminus.parana.cache.CategoryAttributeCacher;
import io.terminus.parana.item.dto.FullItem;
import io.terminus.parana.rule.attribute.SkuAttributeRuleByCategoryExecutor;
import io.terminus.parana.rule.dto.BaseInput;
import java.util.List;

public class ItemSkuAttributeRuleByCategoryExecutor extends SkuAttributeRuleByCategoryExecutor {
   private final CategoryAttributeCacher categoryAttributeCacher;

   public ItemSkuAttributeRuleByCategoryExecutor(CategoryAttributeCacher categoryAttributeCacher) {
      this.categoryAttributeCacher = categoryAttributeCacher;
   }

   protected List getCategoryAttributes(Long categoryId) {
      return this.categoryAttributeCacher.findByCategoryId(categoryId);
   }

   public boolean support(BaseInput input) {
      return input instanceof FullItem;
   }
}

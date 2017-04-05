package io.terminus.parana.rule.sku;

import io.terminus.parana.cache.CategoryAttributeCacher;
import io.terminus.parana.item.dto.FullItem;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.rule.sku.GeneralSkuRuleByCategoryExecutor;
import java.util.List;

public class SkuRuleByCategoryExecutor extends GeneralSkuRuleByCategoryExecutor {
   private final CategoryAttributeCacher categoryAttributeCacher;

   public SkuRuleByCategoryExecutor(CategoryAttributeCacher categoryAttributeCacher) {
      this.categoryAttributeCacher = categoryAttributeCacher;
   }

   protected List getCategoryAttributes(Long categoryId) {
      return this.categoryAttributeCacher.findByCategoryId(categoryId);
   }

   public boolean support(BaseInput input) {
      return input instanceof FullItem;
   }
}

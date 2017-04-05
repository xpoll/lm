package io.terminus.parana.rule.sku;

import io.terminus.parana.component.attribute.CategoryAttributeNoCacher;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.rule.sku.GeneralSkuRuleByCategoryExecutor;
import io.terminus.parana.spu.dto.FullSpu;
import java.util.List;

public class SkuTemplateRuleByCategoryExecutor extends GeneralSkuRuleByCategoryExecutor {
   private final CategoryAttributeNoCacher categoryAttributeNoCacher;

   public SkuTemplateRuleByCategoryExecutor(CategoryAttributeNoCacher categoryAttributeNoCacher) {
      this.categoryAttributeNoCacher = categoryAttributeNoCacher;
   }

   protected List getCategoryAttributes(Long categoryId) {
      return this.categoryAttributeNoCacher.findCategoryAttributeByCategoryId(categoryId);
   }

   public boolean support(BaseInput input) {
      return input instanceof FullSpu;
   }
}

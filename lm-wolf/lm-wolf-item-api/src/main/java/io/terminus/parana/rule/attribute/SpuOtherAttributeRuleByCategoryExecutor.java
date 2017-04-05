package io.terminus.parana.rule.attribute;

import io.terminus.parana.component.attribute.CategoryAttributeNoCacher;
import io.terminus.parana.rule.attribute.OtherAttributeRuleByCategoryExecutor;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.spu.dto.FullSpu;
import java.util.List;

public class SpuOtherAttributeRuleByCategoryExecutor extends OtherAttributeRuleByCategoryExecutor {
   private final CategoryAttributeNoCacher categoryAttributeNoCacher;

   public SpuOtherAttributeRuleByCategoryExecutor(CategoryAttributeNoCacher categoryAttributeNoCacher) {
      this.categoryAttributeNoCacher = categoryAttributeNoCacher;
   }

   protected List getCategoryAttributes(Long categoryId) {
      return this.categoryAttributeNoCacher.findCategoryAttributeByCategoryId(categoryId);
   }

   public boolean support(BaseInput input) {
      return input instanceof FullSpu;
   }
}

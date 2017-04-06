package cn.blmdz.wolf.parana.rule.attribute;

import java.util.List;

import cn.blmdz.wolf.parana.component.attribute.CategoryAttributeNoCacher;
import cn.blmdz.wolf.parana.rule.attribute.SkuAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.parana.rule.dto.BaseInput;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;

public class SpuSkuAttributeRuleByCategoryExecutor extends SkuAttributeRuleByCategoryExecutor {
   private final CategoryAttributeNoCacher categoryAttributeNoCacher;

   public SpuSkuAttributeRuleByCategoryExecutor(CategoryAttributeNoCacher categoryAttributeNoCacher) {
      this.categoryAttributeNoCacher = categoryAttributeNoCacher;
   }

   protected List getCategoryAttributes(Long categoryId) {
      return this.categoryAttributeNoCacher.findCategoryAttributeByCategoryId(categoryId);
   }

   public boolean support(BaseInput input) {
      return input instanceof FullSpu;
   }
}

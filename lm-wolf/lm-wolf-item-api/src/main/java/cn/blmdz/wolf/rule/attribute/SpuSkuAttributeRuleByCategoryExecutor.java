package cn.blmdz.wolf.rule.attribute;

import java.util.List;

import cn.blmdz.wolf.component.attribute.CategoryAttributeNoCacher;
import cn.blmdz.wolf.rule.attribute.SkuAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.spu.dto.FullSpu;

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

package cn.blmdz.wolf.rule.sku;

import java.util.List;

import cn.blmdz.wolf.component.attribute.CategoryAttributeNoCacher;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.rule.sku.GeneralSkuRuleByCategoryExecutor;
import cn.blmdz.wolf.spu.dto.FullSpu;

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

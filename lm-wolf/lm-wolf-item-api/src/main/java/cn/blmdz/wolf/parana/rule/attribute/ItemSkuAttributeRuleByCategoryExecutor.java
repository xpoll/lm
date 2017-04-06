package cn.blmdz.wolf.parana.rule.attribute;

import java.util.List;

import cn.blmdz.wolf.parana.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.rule.attribute.SkuAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.parana.rule.dto.BaseInput;

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

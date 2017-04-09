package cn.blmdz.wolf.rule.attribute;

import java.util.List;

import cn.blmdz.wolf.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.item.dto.FullItem;
import cn.blmdz.wolf.rule.attribute.OtherAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.dto.BaseInput;

public class ItemOtherAttributeRuleByCategoryExecutor extends OtherAttributeRuleByCategoryExecutor {
   private final CategoryAttributeCacher categoryAttributeCacher;

   public ItemOtherAttributeRuleByCategoryExecutor(CategoryAttributeCacher categoryAttributeCacher) {
      this.categoryAttributeCacher = categoryAttributeCacher;
   }

   protected List getCategoryAttributes(Long categoryId) {
      return this.categoryAttributeCacher.findByCategoryId(categoryId);
   }

   public boolean support(BaseInput input) {
      return input instanceof FullItem;
   }
}

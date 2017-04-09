package cn.blmdz.wolf.rule.sku;

import java.util.List;

import cn.blmdz.wolf.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.item.dto.FullItem;
import cn.blmdz.wolf.rule.dto.BaseInput;
import cn.blmdz.wolf.rule.sku.GeneralSkuRuleByCategoryExecutor;

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

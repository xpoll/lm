package cn.blmdz.wolf.parana.rule.sku;

import java.util.List;

import cn.blmdz.wolf.parana.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.rule.dto.BaseInput;
import cn.blmdz.wolf.parana.rule.sku.GeneralSkuRuleByCategoryExecutor;

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

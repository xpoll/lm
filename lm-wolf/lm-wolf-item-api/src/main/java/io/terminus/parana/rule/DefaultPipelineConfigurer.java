package io.terminus.parana.rule;

import io.terminus.parana.cache.CategoryAttributeCacher;
import io.terminus.parana.cache.SpuCacher;
import io.terminus.parana.component.attribute.CategoryAttributeNoCacher;
import io.terminus.parana.rule.PipelineConfigurer;
import io.terminus.parana.rule.RuleExecutorRegistry;
import io.terminus.parana.rule.attribute.AttributeLiteralRule;
import io.terminus.parana.rule.attribute.ItemOtherAttributeRuleByCategoryExecutor;
import io.terminus.parana.rule.attribute.ItemSkuAttributeRuleByCategoryExecutor;
import io.terminus.parana.rule.attribute.OtherAttributeRuleBySpuExecutor;
import io.terminus.parana.rule.attribute.SkuAttributeRuleBySpuExecutor;
import io.terminus.parana.rule.attribute.SpuOtherAttributeRuleByCategoryExecutor;
import io.terminus.parana.rule.attribute.SpuSkuAttributeRuleByCategoryExecutor;
import io.terminus.parana.rule.sku.SkuRuleByCategoryExecutor;
import io.terminus.parana.rule.sku.SkuRuleByGroupedSkuAttributeExecutor;
import io.terminus.parana.rule.sku.SkuRuleBySpuExecutor;
import io.terminus.parana.rule.sku.SkuTemplateRuleByCategoryExecutor;

public class DefaultPipelineConfigurer implements PipelineConfigurer {
   private final SpuCacher spuCacher;
   private final CategoryAttributeCacher categoryAttributeCacher;
   private final CategoryAttributeNoCacher categoryAttributeNoCacher;

   public DefaultPipelineConfigurer(SpuCacher spuCacher, CategoryAttributeCacher categoryAttributeCacher, CategoryAttributeNoCacher categoryAttributeNoCacher) {
      this.spuCacher = spuCacher;
      this.categoryAttributeCacher = categoryAttributeCacher;
      this.categoryAttributeNoCacher = categoryAttributeNoCacher;
   }

   public void configureRuleExecutors(RuleExecutorRegistry ruleExecutorRegistry) {
      ruleExecutorRegistry.register(new AttributeLiteralRule());
      ruleExecutorRegistry.register(new OtherAttributeRuleBySpuExecutor(this.spuCacher));
      ruleExecutorRegistry.register(new ItemOtherAttributeRuleByCategoryExecutor(this.categoryAttributeCacher));
      ruleExecutorRegistry.register(new SpuOtherAttributeRuleByCategoryExecutor(this.categoryAttributeNoCacher));
      ruleExecutorRegistry.register(new SkuAttributeRuleBySpuExecutor(this.spuCacher));
      ruleExecutorRegistry.register(new ItemSkuAttributeRuleByCategoryExecutor(this.categoryAttributeCacher));
      ruleExecutorRegistry.register(new SpuSkuAttributeRuleByCategoryExecutor(this.categoryAttributeNoCacher));
      ruleExecutorRegistry.register(new SkuRuleByGroupedSkuAttributeExecutor());
      ruleExecutorRegistry.register(new SkuRuleBySpuExecutor(this.spuCacher));
      ruleExecutorRegistry.register(new SkuRuleByCategoryExecutor(this.categoryAttributeCacher));
      ruleExecutorRegistry.register(new SkuTemplateRuleByCategoryExecutor(this.categoryAttributeNoCacher));
   }
}

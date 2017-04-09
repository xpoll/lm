package cn.blmdz.wolf.rule;

import cn.blmdz.wolf.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.cache.SpuCacher;
import cn.blmdz.wolf.component.attribute.CategoryAttributeNoCacher;
import cn.blmdz.wolf.rule.PipelineConfigurer;
import cn.blmdz.wolf.rule.RuleExecutorRegistry;
import cn.blmdz.wolf.rule.attribute.AttributeLiteralRule;
import cn.blmdz.wolf.rule.attribute.ItemOtherAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.attribute.ItemSkuAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.attribute.OtherAttributeRuleBySpuExecutor;
import cn.blmdz.wolf.rule.attribute.SkuAttributeRuleBySpuExecutor;
import cn.blmdz.wolf.rule.attribute.SpuOtherAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.attribute.SpuSkuAttributeRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.sku.SkuRuleByCategoryExecutor;
import cn.blmdz.wolf.rule.sku.SkuRuleByGroupedSkuAttributeExecutor;
import cn.blmdz.wolf.rule.sku.SkuRuleBySpuExecutor;
import cn.blmdz.wolf.rule.sku.SkuTemplateRuleByCategoryExecutor;

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

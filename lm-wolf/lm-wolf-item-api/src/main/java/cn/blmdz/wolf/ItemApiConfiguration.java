package cn.blmdz.wolf;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.cache.SpuCacher;
import cn.blmdz.wolf.component.attribute.CategoryAttributeNoCacher;
import cn.blmdz.wolf.rule.DefaultPipelineConfigurer;
import cn.blmdz.wolf.rule.PipelineConfigurer;
import cn.blmdz.wolf.rule.RuleExecutorRegistry;

@Configuration
@ComponentScan({"cn.blmdz.wolf.cache", "cn.blmdz.wolf.rule", "cn.blmdz.wolf.component"})
public class ItemApiConfiguration {
   @ConditionalOnMissingBean({PipelineConfigurer.class})
   @Configuration
   public static class PipelineConfiguration {
      @Bean
      public PipelineConfigurer pipelineConfigurer(SpuCacher spuCacher, CategoryAttributeCacher categoryAttributeCacher, CategoryAttributeNoCacher categoryAttributeNoCacher, RuleExecutorRegistry ruleExecutorRegistry) {
         PipelineConfigurer pipelineConfigurer = new DefaultPipelineConfigurer(spuCacher, categoryAttributeCacher, categoryAttributeNoCacher);
         pipelineConfigurer.configureRuleExecutors(ruleExecutorRegistry);
         return pipelineConfigurer;
      }
   }
}

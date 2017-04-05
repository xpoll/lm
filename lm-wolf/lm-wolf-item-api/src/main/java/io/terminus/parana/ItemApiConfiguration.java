package io.terminus.parana;

import io.terminus.parana.cache.CategoryAttributeCacher;
import io.terminus.parana.cache.SpuCacher;
import io.terminus.parana.component.attribute.CategoryAttributeNoCacher;
import io.terminus.parana.rule.DefaultPipelineConfigurer;
import io.terminus.parana.rule.PipelineConfigurer;
import io.terminus.parana.rule.RuleExecutorRegistry;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"io.terminus.parana.cache", "io.terminus.parana.rule", "io.terminus.parana.component"})
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

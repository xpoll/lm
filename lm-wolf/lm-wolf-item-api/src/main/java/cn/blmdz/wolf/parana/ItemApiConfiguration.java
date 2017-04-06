package cn.blmdz.wolf.parana;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.wolf.parana.cache.CategoryAttributeCacher;
import cn.blmdz.wolf.parana.cache.SpuCacher;
import cn.blmdz.wolf.parana.component.attribute.CategoryAttributeNoCacher;
import cn.blmdz.wolf.parana.rule.DefaultPipelineConfigurer;
import cn.blmdz.wolf.parana.rule.PipelineConfigurer;
import cn.blmdz.wolf.parana.rule.RuleExecutorRegistry;

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

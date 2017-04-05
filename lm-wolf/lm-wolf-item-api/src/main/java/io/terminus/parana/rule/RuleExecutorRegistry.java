package io.terminus.parana.rule;

import io.terminus.parana.rule.RuleExecutor;
import io.terminus.parana.rule.RuleExecutorPipeline;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RuleExecutorRegistry {
   private final RuleExecutorPipeline ruleExecutorPipeline;

   @Autowired
   public RuleExecutorRegistry(RuleExecutorPipeline ruleExecutorPipeline) {
      this.ruleExecutorPipeline = ruleExecutorPipeline;
   }

   public void register(RuleExecutor ruleExecutor) {
      this.ruleExecutorPipeline.add(ruleExecutor);
   }
}

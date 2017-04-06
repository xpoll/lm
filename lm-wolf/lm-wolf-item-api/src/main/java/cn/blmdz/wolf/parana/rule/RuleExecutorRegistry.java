package cn.blmdz.wolf.parana.rule;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.parana.rule.RuleExecutor;
import cn.blmdz.wolf.parana.rule.RuleExecutorPipeline;

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

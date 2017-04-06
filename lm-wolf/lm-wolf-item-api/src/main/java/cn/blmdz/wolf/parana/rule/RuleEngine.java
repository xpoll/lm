package cn.blmdz.wolf.parana.rule;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.parana.rule.dto.BaseInput;
import cn.blmdz.wolf.parana.rule.dto.BaseOutput;
import cn.blmdz.wolf.parana.rule.exception.InvalidException;

@Component
public class RuleEngine {
   private final RuleExecutorPipeline ruleExecutorPipeline;

   @Autowired
   public RuleEngine(RuleExecutorPipeline ruleExecutorPipeline) {
      this.ruleExecutorPipeline = ruleExecutorPipeline;
   }

   public void handleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      for(RuleExecutor ruleExecutor : this.ruleExecutorPipeline.getRuleExecutors()) {
         ruleExecutor.handleInboundData(input, output);
      }

   }

   public void handleOutboundData(BaseInput input, BaseOutput output) {
      for(RuleExecutor ruleExecutor : this.ruleExecutorPipeline.getRuleExecutors()) {
         ruleExecutor.handleOutboundData(input, output);
      }

   }
}

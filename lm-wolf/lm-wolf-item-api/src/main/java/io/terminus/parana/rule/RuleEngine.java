package io.terminus.parana.rule;

import io.terminus.parana.rule.RuleExecutor;
import io.terminus.parana.rule.RuleExecutorPipeline;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.rule.dto.BaseOutput;
import io.terminus.parana.rule.exception.InvalidException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

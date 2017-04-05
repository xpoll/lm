package io.terminus.parana.rule;

import io.terminus.parana.rule.RuleExecutor;
import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
class RuleExecutorPipeline {
   private final List ruleExecutors = new ArrayList();

   public List getRuleExecutors() {
      return this.ruleExecutors;
   }

   public void clear() {
      this.ruleExecutors.clear();
   }

   protected void addAll(List ruleExecutors) {
      ruleExecutors.addAll(ruleExecutors);
   }

   protected void add(RuleExecutor ruleExecutor) {
      this.ruleExecutors.add(ruleExecutor);
   }
}

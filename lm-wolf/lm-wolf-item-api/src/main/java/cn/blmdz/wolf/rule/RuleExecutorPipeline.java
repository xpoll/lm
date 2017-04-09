package cn.blmdz.wolf.rule;

import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.rule.RuleExecutor;

@Component
class RuleExecutorPipeline {
   private final List<RuleExecutor> ruleExecutors = new ArrayList();

   public List<RuleExecutor> getRuleExecutors() {
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

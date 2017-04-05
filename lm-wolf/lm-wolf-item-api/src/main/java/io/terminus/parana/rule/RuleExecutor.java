package io.terminus.parana.rule;

import io.terminus.parana.attribute.dto.AttributeMetaKey;
import io.terminus.parana.rule.dto.BaseInput;
import io.terminus.parana.rule.dto.BaseOutput;
import io.terminus.parana.rule.exception.InvalidException;
import java.util.List;
import java.util.Map;
import org.springframework.util.CollectionUtils;

public abstract class RuleExecutor {
   public abstract void doHandleInboundData(BaseInput var1, BaseOutput var2) throws InvalidException;

   public abstract void doHandleOutboundData(BaseInput var1, BaseOutput var2);

   public abstract boolean support(BaseInput var1);

   protected void handleInboundData(BaseInput input, BaseOutput output) throws InvalidException {
      if(this.support(input)) {
         this.doHandleInboundData(input, output);
      }

   }

   protected void handleOutboundData(BaseInput data, BaseOutput output) {
      if(this.support(data)) {
         this.doHandleOutboundData(data, output);
      }

   }

   protected abstract List getRules(BaseInput var1);

   protected static boolean userDefinedValueAllowed(Map attrMetas) {
      return CollectionUtils.isEmpty(attrMetas)?false:Boolean.valueOf((String)attrMetas.get(AttributeMetaKey.USER_DEFINED)).booleanValue();
   }
}

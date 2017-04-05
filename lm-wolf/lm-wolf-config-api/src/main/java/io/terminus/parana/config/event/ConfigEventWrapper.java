package io.terminus.parana.config.event;

import com.google.common.base.Optional;
import io.terminus.parana.config.event.ConfigEvent;
import io.terminus.parana.config.event.Operator;
import java.util.List;

public class ConfigEventWrapper {
   public static ConfigEvent wrapperEvent(Operator op, List configs) {
      ConfigEvent event = new ConfigEvent();
      event.setOp(Optional.of(op.value));
      event.setData(Optional.of(configs));
      return event;
   }
}

package cn.blmdz.wolf.config.event;

import com.google.common.base.Optional;

import cn.blmdz.wolf.config.event.ConfigEvent;
import cn.blmdz.wolf.config.event.Operator;

import java.util.List;

public class ConfigEventWrapper {
   public static ConfigEvent wrapperEvent(Operator op, List configs) {
      ConfigEvent event = new ConfigEvent();
      event.setOp(Optional.of(op.value));
      event.setData(Optional.of(configs));
      return event;
   }
}

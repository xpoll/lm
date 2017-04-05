package io.terminus.parana.config.event;

import java.util.Objects;

public enum Operator {
   DELETE(Integer.valueOf(-1)),
   CREATE(Integer.valueOf(0)),
   UPDATE(Integer.valueOf(1));

   public final Integer value;

   public static Operator from(Integer value) {
      for(Operator op : values()) {
         if(Objects.equals(op.value, value)) {
            return op;
         }
      }

      throw new IllegalArgumentException("not an valid operator");
   }

   private Operator(Integer value) {
      this.value = value;
   }
}

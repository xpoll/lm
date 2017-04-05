package io.terminus.parana.settlement.enums;

public enum PayChannelTransType {
   PAID(1, "支付"),
   REJECT_REFUND(2, "拒收退款"),
   AFTER_REFUND(3, "售后退款");

   private final int value;
   private final String description;

   private PayChannelTransType(int value, String description) {
      this.value = value;
      this.description = description;
   }

   public static PayChannelTransType from(int value) {
      for(PayChannelTransType ta : values()) {
         if(ta.value == value) {
            return ta;
         }
      }

      return null;
   }

   public int value() {
      return this.value;
   }

   public String toString() {
      return this.description;
   }
}

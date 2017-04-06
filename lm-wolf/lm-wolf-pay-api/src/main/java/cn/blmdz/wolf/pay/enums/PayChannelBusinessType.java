package cn.blmdz.wolf.pay.enums;

public enum PayChannelBusinessType {
   PAID(1, "支付"),
   REJECT_REFUND(2, "拒收退款"),
   AFTER_REFUND(3, "售后退款");

   private final int value;
   private final String description;

   private PayChannelBusinessType(int value, String description) {
      this.value = value;
      this.description = description;
   }

   public static PayChannelBusinessType from(int value) {
      for(PayChannelBusinessType ta : values()) {
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

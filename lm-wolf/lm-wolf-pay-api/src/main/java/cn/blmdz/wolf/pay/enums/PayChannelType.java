package cn.blmdz.wolf.pay.enums;

public enum PayChannelType {
   PAID(1, "支付"),
   REFUND(2, "退款");

   private final int value;
   private final String description;

   private PayChannelType(int value, String description) {
      this.value = value;
      this.description = description;
   }

   public static PayChannelType from(int value) {
      for(PayChannelType ta : values()) {
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

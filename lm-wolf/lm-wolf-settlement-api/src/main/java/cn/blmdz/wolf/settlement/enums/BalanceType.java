package cn.blmdz.wolf.settlement.enums;

public enum BalanceType {
   PAID(1, "支付"),
   SALE_REFUND(2, "退款"),
   AFTER_SALE_REFUND(3, "售后");

   private final int value;
   private final String description;

   private BalanceType(int value, String description) {
      this.value = value;
      this.description = description;
   }

   public static BalanceType from(int value) {
      for(BalanceType ta : values()) {
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

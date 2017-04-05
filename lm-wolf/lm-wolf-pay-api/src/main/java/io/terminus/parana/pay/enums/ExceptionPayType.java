package io.terminus.parana.pay.enums;

public enum ExceptionPayType {
   TIME_OUT(1, "支付成功是，订单已超时关闭"),
   MULTIPLE_PAY(2, "重复支付");

   private final int value;
   private final String description;

   private ExceptionPayType(int value, String description) {
      this.value = value;
      this.description = description;
   }

   public int value() {
      return this.value;
   }

   public String toString() {
      return this.description;
   }
}

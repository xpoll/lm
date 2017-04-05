package io.terminus.parana.settlement.enums;

public enum CommissionDetailType {
   FINISH_ORDER(1, "完成订单"),
   AFTER_SALE_ORDER(2, "售后订单");

   private final int value;
   private final String description;

   private CommissionDetailType(int value, String description) {
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

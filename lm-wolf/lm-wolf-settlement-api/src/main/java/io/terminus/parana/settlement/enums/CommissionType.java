package io.terminus.parana.settlement.enums;

public enum CommissionType {
   PLATFORM(1, "平台"),
   SHOP(2, "店铺");

   private final int value;
   private final String description;

   private CommissionType(int value, String description) {
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

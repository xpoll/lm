package io.terminus.parana.pay.enums;

public enum OwnerType {
   SITE(1, "站点"),
   SHOP(2, "店铺"),
   PLATFORM(3, "平台");

   private final int value;
   private final String description;

   private OwnerType(int value, String description) {
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

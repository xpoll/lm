package cn.blmdz.wolf.settlement.enums;

public enum CheckStatus {
   WAIT_CHECK(0, "待对账"),
   WAIT_THRID_ACCOUNTS(1, "待拉取第三方账务"),
   CHECK_SUCCESS(2, "对账完成"),
   CHECK_FAIL(-1, "对账失败-账务不等");

   private final int value;
   private final String description;

   private CheckStatus(int value, String description) {
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

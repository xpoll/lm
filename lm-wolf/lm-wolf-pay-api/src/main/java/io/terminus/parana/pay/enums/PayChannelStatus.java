package io.terminus.parana.pay.enums;

public enum PayChannelStatus {
   WAIT_HANDLE(0, "待处理"),
   FINISH(1, "已完成"),
   FAIL(1, "失败");

   private final int value;
   private final String description;

   private PayChannelStatus(int value, String description) {
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

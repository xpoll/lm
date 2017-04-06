package cn.blmdz.wolf.pay.enums;

import com.google.common.base.Objects;

public enum TrackHandleStatus {
   NOT(0, "未处理"),
   YES(1, "已处理"),
   CLOSED(-1, "已关闭");

   private final int value;
   private final String description;

   private TrackHandleStatus(int value, String description) {
      this.value = value;
      this.description = description;
   }

   public static TrackHandleStatus from(int value) {
      for(TrackHandleStatus status : values()) {
         if(Objects.equal(Integer.valueOf(status.value), Integer.valueOf(value))) {
            return status;
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

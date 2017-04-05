package io.terminus.parana.msg.model;

import com.google.common.base.Objects;

public enum Channel {
   Unknown(-1, "未知消息渠道"),
   Notification(0, "站内信"),
   Sms(1, "短信"),
   Email(2, "邮件"),
   AppPush(3, "APP推送消息");

   private final int value;
   private final String desc;

   private Channel(int number, String desc) {
      this.value = number;
      this.desc = desc;
   }

   public static Channel from(int value) {
      for(Channel range : values()) {
         if(Objects.equal(Integer.valueOf(range.value), Integer.valueOf(value))) {
            return range;
         }
      }

      return null;
   }

   public int value() {
      return this.value;
   }

   public String toString() {
      return this.desc;
   }
}

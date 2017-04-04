package cn.blmdz.aide.pay.channel.alipay.enums;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum RefundInterfaceType {
   NEEDPASSWORD("1", "有密"),
   NOPASSWRD("2", "无密");

   private final String value;
   private final String description;

   public String value() {
      return this.value;
   }

   public String toString() {
      return this.description;
   }
}

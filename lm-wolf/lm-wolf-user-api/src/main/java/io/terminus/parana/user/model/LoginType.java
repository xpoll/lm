package io.terminus.parana.user.model;

public enum LoginType {
   NAME(1),
   EMAIL(2),
   MOBILE(3),
   OTHER(4);

   private final int type;

   private LoginType(int type) {
      this.type = type;
   }

   public static LoginType from(int value) {
      for(LoginType loginType : values()) {
         if(loginType.type == value) {
            return loginType;
         }
      }

      throw new IllegalArgumentException("illegal login type: " + value);
   }
}

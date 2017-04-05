package io.terminus.parana.msg.component.validate;

import java.util.regex.Pattern;

public enum CommonPattern {
   EMAIL {
      private Pattern pattern = Pattern.compile("^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$");

      public boolean invalid(String text) {
         return text == null || !this.pattern.matcher(text).matches();
      }
   },
   MOBILE {
      private Pattern pattern = Pattern.compile("^((13[0-9])|(14[0-9])|(15[0-9])|(17[0-9])|(18[0-9]))\\d{8}$");

      public boolean invalid(String text) {
         return text == null || !this.pattern.matcher(text).matches();
      }
   };

   private CommonPattern() {
   }

   public abstract boolean invalid(String var1);
}

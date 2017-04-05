package io.terminus.parana.auth.role;

public class CustomRoleLoadException extends RuntimeException {
   private static final long serialVersionUID = 0L;

   public CustomRoleLoadException() {
   }

   public CustomRoleLoadException(String message) {
      super(message);
   }

   public CustomRoleLoadException(String format, Object... args) {
      super(String.format(format, args));
   }
}

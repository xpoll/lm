package io.terminus.parana.rule.exception;

import com.google.common.base.MoreObjects;

public class InvalidException extends RuntimeException {
   private static final long serialVersionUID = -3978990660036533916L;
   private final int status;
   private final String error;
   private final Object[] params;

   public InvalidException(int status, String error, Object... params) {
      this.status = status;
      this.error = error;
      this.params = params;
   }

   public InvalidException(String error, Object... param) {
      this(400, error, param);
   }

   public InvalidException(String error) {
      this(error, (Object[])null);
   }

   public InvalidException(int status, String error) {
      this(status, error, (Object[])null);
   }

   public String toString() {
      return MoreObjects.toStringHelper(this).add("status", this.status).add("error", this.error).add("params", this.params).omitNullValues().toString();
   }

   public int getStatus() {
      return this.status;
   }

   public String getError() {
      return this.error;
   }

   public Object[] getParams() {
      return this.params;
   }
}

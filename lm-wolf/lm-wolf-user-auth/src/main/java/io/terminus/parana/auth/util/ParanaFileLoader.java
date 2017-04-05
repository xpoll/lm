package io.terminus.parana.auth.util;

import com.google.common.base.Charsets;

public interface ParanaFileLoader {
   ParanaFileLoader.Resp load(String var1);

   ParanaFileLoader.Resp load(String var1, String var2);

   public static class Resp {
      public static final ParanaFileLoader.Resp NOT_FOUND = new ParanaFileLoader.Resp();
      public static final ParanaFileLoader.Resp NOT_MODIFIED = new ParanaFileLoader.Resp();
      private boolean notFound;
      private boolean notModified;
      private String sign;
      private byte[] context;
      private String contextString;

      public byte[] getContext() {
         if(this.context == null) {
            this.context = this.contextString.getBytes(Charsets.UTF_8);
         }

         return this.context;
      }

      public String asString() {
         if(this.contextString == null) {
            this.contextString = new String(this.context, Charsets.UTF_8);
         }

         return this.contextString;
      }

      public boolean modified() {
         return !this.notModified;
      }

      public boolean isNotFound() {
         return this.notFound;
      }

      public void setNotFound(boolean notFound) {
         this.notFound = notFound;
      }

      public boolean isNotModified() {
         return this.notModified;
      }

      public void setNotModified(boolean notModified) {
         this.notModified = notModified;
      }

      public String getSign() {
         return this.sign;
      }

      public void setSign(String sign) {
         this.sign = sign;
      }

      public void setContext(byte[] context) {
         this.context = context;
      }

      public void setContextString(String contextString) {
         this.contextString = contextString;
      }

      static {
         NOT_FOUND.notFound = true;
         NOT_MODIFIED.notModified = true;
      }
   }
}

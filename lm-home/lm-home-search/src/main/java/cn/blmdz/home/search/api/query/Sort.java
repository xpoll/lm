package cn.blmdz.home.search.api.query;

import java.io.Serializable;

public class Sort implements Serializable {
   private static final long serialVersionUID = -5455456189083087970L;
   private final String field;
   private final String order;
   private boolean last;

   public Sort(String field, String order) {
      this.field = field;
      this.order = order;
   }

   public String getField() {
      return this.field;
   }

   public String getOrder() {
      return this.order;
   }

   public boolean isLast() {
      return this.last;
   }

   public void setLast(boolean last) {
      this.last = last;
   }
}

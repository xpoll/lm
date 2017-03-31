package cn.blmdz.home.search.api.query;

import java.io.Serializable;

public class Highlight implements Serializable {
   private static final long serialVersionUID = -5305949090405719551L;
   private final String field;
   private boolean last;

   public Highlight(String field) {
      this.field = field;
   }

   public String getField() {
      return this.field;
   }

   public boolean isLast() {
      return this.last;
   }

   public void setLast(boolean last) {
      this.last = last;
   }
}

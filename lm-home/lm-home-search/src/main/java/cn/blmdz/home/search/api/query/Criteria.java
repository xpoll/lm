package cn.blmdz.home.search.api.query;

import java.io.Serializable;

public abstract class Criteria implements Serializable {
   private static final long serialVersionUID = 9069160600548648345L;
   protected final String field;
   protected boolean last;

   public Criteria(String field) {
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
